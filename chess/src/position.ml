open Core_kernel
open Monads.Std

(* To avoid allocation, we use the unboxed options. *)
module Uopt = struct
  include Uopt

  (* Required for `@@deriving compare, equal, sexp` *)

  let compare cmp x y = Option.compare cmp (to_option x) (to_option y)
  let equal eq x y = Option.equal eq (to_option x) (to_option y)
  let sexp_of_t f x = Option.sexp_of_t f @@ to_option x
  let t_of_sexp f x = of_option @@ Option.t_of_sexp f x

  (* Convenience functions. *)

  let[@inline] map x ~f = if is_none x then x else some @@ f @@ unsafe_value x

  let[@inline] value_map x ~default ~f =
    if is_none x then default else f @@ unsafe_value x

  let[@inline] iter x ~f = if not @@ is_none x then f @@ unsafe_value x
end

module Pre = Precalculated
module Bb = Bitboard
module Cr = Castling_rights

module T = struct
  (* We'll use mutable fields since, when applying moves, this has a
     performance advantage over a typical state monad pattern (where
     we are making a new copy every time we update a field). *)
  type t = {
    mutable white : Bb.t;
    mutable black : Bb.t;
    mutable pawn : Bb.t;
    mutable knight : Bb.t;
    mutable bishop : Bb.t;
    mutable rook : Bb.t;
    mutable queen : Bb.t;
    mutable king : Bb.t;
    mutable active : Piece.color;
    mutable castle : Cr.t;
    mutable en_passant : Square.t Uopt.t;
    mutable halfmove : int;
    mutable fullmove : int;
    mutable hash : int64;
    mutable pawn_hash : int64;
  } [@@deriving compare, equal, fields, sexp]

  let[@inline] en_passant pos = Uopt.to_option pos.en_passant

  (* We make an explicit copy because our move generator will return
     a new position (thus adhering to a functional style). *)
  let[@inline] copy pos = {
    white      = pos.white;
    black      = pos.black;
    pawn       = pos.pawn;
    knight     = pos.knight;
    bishop     = pos.bishop;
    rook       = pos.rook;
    queen      = pos.queen;
    king       = pos.king;
    active     = pos.active;
    castle     = pos.castle;
    en_passant = pos.en_passant;
    halfmove   = pos.halfmove;
    fullmove   = pos.fullmove;
    hash       = pos.hash;
    pawn_hash  = pos.pawn_hash;
  }
end

include T

let[@inline] inactive pos = Piece.Color.opposite pos.active

(* Bitboard accessors *)

let[@inline] all_board pos = Bb.(pos.white + pos.black)

let[@inline] board_of_color pos = function
  | Piece.White -> pos.white
  | Piece.Black -> pos.black

let[@inline] active_board pos = board_of_color pos pos.active
let[@inline] inactive_board pos = board_of_color pos @@ inactive pos

let[@inline] board_of_kind pos = function
  | Piece.Pawn -> pos.pawn
  | Piece.Knight -> pos.knight
  | Piece.Bishop -> pos.bishop
  | Piece.Rook -> pos.rook
  | Piece.Queen -> pos.queen
  | Piece.King -> pos.king

let[@inline] board_of_piece pos p =
  let c, k = Piece.decomp p in
  Bb.(board_of_color pos c & board_of_kind pos k)

let[@inline] is_en_passant pos sq =
  Uopt.is_some pos.en_passant &&
  Square.(sq = Uopt.unsafe_value pos.en_passant) 

let[@inline] en_passant_pawn_aux active ep = match active with
  | Piece.White -> Square.(with_rank_unsafe ep Rank.five)
  | Piece.Black -> Square.(with_rank_unsafe ep Rank.four)

let[@inline] en_passant_pawn_uopt pos =
  Uopt.map pos.en_passant ~f:(en_passant_pawn_aux pos.active)

let[@inline] en_passant_pawn pos = Uopt.to_option @@ en_passant_pawn_uopt pos

let[@inline] has_en_passant_threat_aux pos ep =
  let cap = Pre.pawn_capture ep @@ Piece.Color.opposite pos.active in
  Bb.((cap & pos.pawn & active_board pos) <> empty)

let[@inline] has_en_passant_threat pos =
  Uopt.value_map pos.en_passant
    ~default:false ~f:(has_en_passant_threat_aux pos)

(* Piece lookup *)

let which_color pos sq =
  let open Bb.Syntax in
  if sq @ pos.white then Uopt.some Piece.White
  else if sq @ pos.black then Uopt.some Piece.Black
  else Uopt.none

let which_color_exn pos sq =
  let open Bb.Syntax in
  if sq @ pos.white then Piece.White
  else if sq @ pos.black then Piece.Black
  else invalid_argf "No piece exists at square %s" (Square.to_string sq) ()

let which_kind pos sq =
  let open Bb.Syntax in
  if sq @ pos.pawn then Uopt.some Piece.Pawn
  else if sq @ pos.knight then Uopt.some Piece.Knight
  else if sq @ pos.bishop then Uopt.some Piece.Bishop
  else if sq @ pos.rook then Uopt.some Piece.Rook
  else if sq @ pos.queen then Uopt.some Piece.Queen
  else if sq @ pos.king then Uopt.some Piece.King
  else Uopt.none

let which_kind_exn pos sq =
  let open Bb.Syntax in
  if sq @ pos.pawn then Piece.Pawn
  else if sq @ pos.knight then Piece.Knight
  else if sq @ pos.bishop then Piece.Bishop
  else if sq @ pos.rook then Piece.Rook
  else if sq @ pos.queen then Piece.Queen
  else if sq @ pos.king then Piece.King
  else invalid_argf "No piece exists at square %s" (Square.to_string sq) ()

let collect_color pos c =
  board_of_color pos c |> Bb.fold ~init:[] ~f:(fun acc sq ->
      (sq, which_kind_exn pos sq) :: acc)

let collect_active pos = collect_color pos pos.active
let collect_inactive pos = collect_color pos @@ inactive pos

let collect_kind pos k =
  board_of_kind pos k |> Bb.fold ~init:[] ~f:(fun acc sq ->
      (sq, which_color_exn pos sq) :: acc)

let collect_piece pos p =
  board_of_piece pos p |> Bb.fold ~init:[] ~f:(fun acc sq -> sq :: acc)

let piece_at_square_uopt pos sq =
  let c = which_color pos sq in
  if Uopt.is_none c then Uopt.none
  else
    let c = Uopt.unsafe_value c in
    let k = which_kind pos sq in
    if Uopt.is_none k then
      failwithf "Square %s is set for color %s, but no piece kind is available"
        (Square.to_string sq) (Piece.Color.to_string_hum c) ()
    else
      let k = Uopt.unsafe_value k in
      Uopt.some @@ Piece.create c k

let piece_at_square pos sq = Uopt.to_option @@ piece_at_square_uopt pos sq

let piece_at_square_exn pos sq =
  Piece.create (which_color_exn pos sq) (which_kind_exn pos sq)

let collect_all pos =
  all_board pos |> Bb.fold ~init:[] ~f:(fun acc sq ->
      let c = which_color_exn pos sq in
      let k = which_kind_exn pos sq in
      (sq, Piece.create c k) :: acc)

(* Zobrist hashing *)

module Hash = struct
  (* The randomly generated keys for hashing positions. These constants are
     used for the Polyglot format. We could generate our own random keys, but
     we would also like to enable code re-use with the opening book module.

     If there is any provenance as to how these numbers were generated, then
     we would prefer to use that method instead of hardcoding them.
  *)
  module Keys = struct
    let piece_keys = [|
      0x9D39247E33776D41L; 0x2AF7398005AAA5C7L; 0x44DB015024623547L; 0x9C15F73E62A76AE2L;
      0x75834465489C0C89L; 0x3290AC3A203001BFL; 0x0FBBAD1F61042279L; 0xE83A908FF2FB60CAL;
      0x0D7E765D58755C10L; 0x1A083822CEAFE02DL; 0x9605D5F0E25EC3B0L; 0xD021FF5CD13A2ED5L;
      0x40BDF15D4A672E32L; 0x011355146FD56395L; 0x5DB4832046F3D9E5L; 0x239F8B2D7FF719CCL;
      0x05D1A1AE85B49AA1L; 0x679F848F6E8FC971L; 0x7449BBFF801FED0BL; 0x7D11CDB1C3B7ADF0L;
      0x82C7709E781EB7CCL; 0xF3218F1C9510786CL; 0x331478F3AF51BBE6L; 0x4BB38DE5E7219443L;
      0xAA649C6EBCFD50FCL; 0x8DBD98A352AFD40BL; 0x87D2074B81D79217L; 0x19F3C751D3E92AE1L;
      0xB4AB30F062B19ABFL; 0x7B0500AC42047AC4L; 0xC9452CA81A09D85DL; 0x24AA6C514DA27500L;
      0x4C9F34427501B447L; 0x14A68FD73C910841L; 0xA71B9B83461CBD93L; 0x03488B95B0F1850FL;
      0x637B2B34FF93C040L; 0x09D1BC9A3DD90A94L; 0x3575668334A1DD3BL; 0x735E2B97A4C45A23L;
      0x18727070F1BD400BL; 0x1FCBACD259BF02E7L; 0xD310A7C2CE9B6555L; 0xBF983FE0FE5D8244L;
      0x9F74D14F7454A824L; 0x51EBDC4AB9BA3035L; 0x5C82C505DB9AB0FAL; 0xFCF7FE8A3430B241L;
      0x3253A729B9BA3DDEL; 0x8C74C368081B3075L; 0xB9BC6C87167C33E7L; 0x7EF48F2B83024E20L;
      0x11D505D4C351BD7FL; 0x6568FCA92C76A243L; 0x4DE0B0F40F32A7B8L; 0x96D693460CC37E5DL;
      0x42E240CB63689F2FL; 0x6D2BDCDAE2919661L; 0x42880B0236E4D951L; 0x5F0F4A5898171BB6L;
      0x39F890F579F92F88L; 0x93C5B5F47356388BL; 0x63DC359D8D231B78L; 0xEC16CA8AEA98AD76L;
      0x5355F900C2A82DC7L; 0x07FB9F855A997142L; 0x5093417AA8A7ED5EL; 0x7BCBC38DA25A7F3CL;
      0x19FC8A768CF4B6D4L; 0x637A7780DECFC0D9L; 0x8249A47AEE0E41F7L; 0x79AD695501E7D1E8L;
      0x14ACBAF4777D5776L; 0xF145B6BECCDEA195L; 0xDABF2AC8201752FCL; 0x24C3C94DF9C8D3F6L;
      0xBB6E2924F03912EAL; 0x0CE26C0B95C980D9L; 0xA49CD132BFBF7CC4L; 0xE99D662AF4243939L;
      0x27E6AD7891165C3FL; 0x8535F040B9744FF1L; 0x54B3F4FA5F40D873L; 0x72B12C32127FED2BL;
      0xEE954D3C7B411F47L; 0x9A85AC909A24EAA1L; 0x70AC4CD9F04F21F5L; 0xF9B89D3E99A075C2L;
      0x87B3E2B2B5C907B1L; 0xA366E5B8C54F48B8L; 0xAE4A9346CC3F7CF2L; 0x1920C04D47267BBDL;
      0x87BF02C6B49E2AE9L; 0x092237AC237F3859L; 0xFF07F64EF8ED14D0L; 0x8DE8DCA9F03CC54EL;
      0x9C1633264DB49C89L; 0xB3F22C3D0B0B38EDL; 0x390E5FB44D01144BL; 0x5BFEA5B4712768E9L;
      0x1E1032911FA78984L; 0x9A74ACB964E78CB3L; 0x4F80F7A035DAFB04L; 0x6304D09A0B3738C4L;
      0x2171E64683023A08L; 0x5B9B63EB9CEFF80CL; 0x506AACF489889342L; 0x1881AFC9A3A701D6L;
      0x6503080440750644L; 0xDFD395339CDBF4A7L; 0xEF927DBCF00C20F2L; 0x7B32F7D1E03680ECL;
      0xB9FD7620E7316243L; 0x05A7E8A57DB91B77L; 0xB5889C6E15630A75L; 0x4A750A09CE9573F7L;
      0xCF464CEC899A2F8AL; 0xF538639CE705B824L; 0x3C79A0FF5580EF7FL; 0xEDE6C87F8477609DL;
      0x799E81F05BC93F31L; 0x86536B8CF3428A8CL; 0x97D7374C60087B73L; 0xA246637CFF328532L;
      0x043FCAE60CC0EBA0L; 0x920E449535DD359EL; 0x70EB093B15B290CCL; 0x73A1921916591CBDL;
      0x56436C9FE1A1AA8DL; 0xEFAC4B70633B8F81L; 0xBB215798D45DF7AFL; 0x45F20042F24F1768L;
      0x930F80F4E8EB7462L; 0xFF6712FFCFD75EA1L; 0xAE623FD67468AA70L; 0xDD2C5BC84BC8D8FCL;
      0x7EED120D54CF2DD9L; 0x22FE545401165F1CL; 0xC91800E98FB99929L; 0x808BD68E6AC10365L;
      0xDEC468145B7605F6L; 0x1BEDE3A3AEF53302L; 0x43539603D6C55602L; 0xAA969B5C691CCB7AL;
      0xA87832D392EFEE56L; 0x65942C7B3C7E11AEL; 0xDED2D633CAD004F6L; 0x21F08570F420E565L;
      0xB415938D7DA94E3CL; 0x91B859E59ECB6350L; 0x10CFF333E0ED804AL; 0x28AED140BE0BB7DDL;
      0xC5CC1D89724FA456L; 0x5648F680F11A2741L; 0x2D255069F0B7DAB3L; 0x9BC5A38EF729ABD4L;
      0xEF2F054308F6A2BCL; 0xAF2042F5CC5C2858L; 0x480412BAB7F5BE2AL; 0xAEF3AF4A563DFE43L;
      0x19AFE59AE451497FL; 0x52593803DFF1E840L; 0xF4F076E65F2CE6F0L; 0x11379625747D5AF3L;
      0xBCE5D2248682C115L; 0x9DA4243DE836994FL; 0x066F70B33FE09017L; 0x4DC4DE189B671A1CL;
      0x51039AB7712457C3L; 0xC07A3F80C31FB4B4L; 0xB46EE9C5E64A6E7CL; 0xB3819A42ABE61C87L;
      0x21A007933A522A20L; 0x2DF16F761598AA4FL; 0x763C4A1371B368FDL; 0xF793C46702E086A0L;
      0xD7288E012AEB8D31L; 0xDE336A2A4BC1C44BL; 0x0BF692B38D079F23L; 0x2C604A7A177326B3L;
      0x4850E73E03EB6064L; 0xCFC447F1E53C8E1BL; 0xB05CA3F564268D99L; 0x9AE182C8BC9474E8L;
      0xA4FC4BD4FC5558CAL; 0xE755178D58FC4E76L; 0x69B97DB1A4C03DFEL; 0xF9B5B7C4ACC67C96L;
      0xFC6A82D64B8655FBL; 0x9C684CB6C4D24417L; 0x8EC97D2917456ED0L; 0x6703DF9D2924E97EL;
      0xC547F57E42A7444EL; 0x78E37644E7CAD29EL; 0xFE9A44E9362F05FAL; 0x08BD35CC38336615L;
      0x9315E5EB3A129ACEL; 0x94061B871E04DF75L; 0xDF1D9F9D784BA010L; 0x3BBA57B68871B59DL;
      0xD2B7ADEEDED1F73FL; 0xF7A255D83BC373F8L; 0xD7F4F2448C0CEB81L; 0xD95BE88CD210FFA7L;
      0x336F52F8FF4728E7L; 0xA74049DAC312AC71L; 0xA2F61BB6E437FDB5L; 0x4F2A5CB07F6A35B3L;
      0x87D380BDA5BF7859L; 0x16B9F7E06C453A21L; 0x7BA2484C8A0FD54EL; 0xF3A678CAD9A2E38CL;
      0x39B0BF7DDE437BA2L; 0xFCAF55C1BF8A4424L; 0x18FCF680573FA594L; 0x4C0563B89F495AC3L;
      0x40E087931A00930DL; 0x8CFFA9412EB642C1L; 0x68CA39053261169FL; 0x7A1EE967D27579E2L;
      0x9D1D60E5076F5B6FL; 0x3810E399B6F65BA2L; 0x32095B6D4AB5F9B1L; 0x35CAB62109DD038AL;
      0xA90B24499FCFAFB1L; 0x77A225A07CC2C6BDL; 0x513E5E634C70E331L; 0x4361C0CA3F692F12L;
      0xD941ACA44B20A45BL; 0x528F7C8602C5807BL; 0x52AB92BEB9613989L; 0x9D1DFA2EFC557F73L;
      0x722FF175F572C348L; 0x1D1260A51107FE97L; 0x7A249A57EC0C9BA2L; 0x04208FE9E8F7F2D6L;
      0x5A110C6058B920A0L; 0x0CD9A497658A5698L; 0x56FD23C8F9715A4CL; 0x284C847B9D887AAEL;
      0x04FEABFBBDB619CBL; 0x742E1E651C60BA83L; 0x9A9632E65904AD3CL; 0x881B82A13B51B9E2L;
      0x506E6744CD974924L; 0xB0183DB56FFC6A79L; 0x0ED9B915C66ED37EL; 0x5E11E86D5873D484L;
      0xF678647E3519AC6EL; 0x1B85D488D0F20CC5L; 0xDAB9FE6525D89021L; 0x0D151D86ADB73615L;
      0xA865A54EDCC0F019L; 0x93C42566AEF98FFBL; 0x99E7AFEABE000731L; 0x48CBFF086DDF285AL;
      0x7F9B6AF1EBF78BAFL; 0x58627E1A149BBA21L; 0x2CD16E2ABD791E33L; 0xD363EFF5F0977996L;
      0x0CE2A38C344A6EEDL; 0x1A804AADB9CFA741L; 0x907F30421D78C5DEL; 0x501F65EDB3034D07L;
      0x37624AE5A48FA6E9L; 0x957BAF61700CFF4EL; 0x3A6C27934E31188AL; 0xD49503536ABCA345L;
      0x088E049589C432E0L; 0xF943AEE7FEBF21B8L; 0x6C3B8E3E336139D3L; 0x364F6FFA464EE52EL;
      0xD60F6DCEDC314222L; 0x56963B0DCA418FC0L; 0x16F50EDF91E513AFL; 0xEF1955914B609F93L;
      0x565601C0364E3228L; 0xECB53939887E8175L; 0xBAC7A9A18531294BL; 0xB344C470397BBA52L;
      0x65D34954DAF3CEBDL; 0xB4B81B3FA97511E2L; 0xB422061193D6F6A7L; 0x071582401C38434DL;
      0x7A13F18BBEDC4FF5L; 0xBC4097B116C524D2L; 0x59B97885E2F2EA28L; 0x99170A5DC3115544L;
      0x6F423357E7C6A9F9L; 0x325928EE6E6F8794L; 0xD0E4366228B03343L; 0x565C31F7DE89EA27L;
      0x30F5611484119414L; 0xD873DB391292ED4FL; 0x7BD94E1D8E17DEBCL; 0xC7D9F16864A76E94L;
      0x947AE053EE56E63CL; 0xC8C93882F9475F5FL; 0x3A9BF55BA91F81CAL; 0xD9A11FBB3D9808E4L;
      0x0FD22063EDC29FCAL; 0xB3F256D8ACA0B0B9L; 0xB03031A8B4516E84L; 0x35DD37D5871448AFL;
      0xE9F6082B05542E4EL; 0xEBFAFA33D7254B59L; 0x9255ABB50D532280L; 0xB9AB4CE57F2D34F3L;
      0x693501D628297551L; 0xC62C58F97DD949BFL; 0xCD454F8F19C5126AL; 0xBBE83F4ECC2BDECBL;
      0xDC842B7E2819E230L; 0xBA89142E007503B8L; 0xA3BC941D0A5061CBL; 0xE9F6760E32CD8021L;
      0x09C7E552BC76492FL; 0x852F54934DA55CC9L; 0x8107FCCF064FCF56L; 0x098954D51FFF6580L;
      0x23B70EDB1955C4BFL; 0xC330DE426430F69DL; 0x4715ED43E8A45C0AL; 0xA8D7E4DAB780A08DL;
      0x0572B974F03CE0BBL; 0xB57D2E985E1419C7L; 0xE8D9ECBE2CF3D73FL; 0x2FE4B17170E59750L;
      0x11317BA87905E790L; 0x7FBF21EC8A1F45ECL; 0x1725CABFCB045B00L; 0x964E915CD5E2B207L;
      0x3E2B8BCBF016D66DL; 0xBE7444E39328A0ACL; 0xF85B2B4FBCDE44B7L; 0x49353FEA39BA63B1L;
      0x1DD01AAFCD53486AL; 0x1FCA8A92FD719F85L; 0xFC7C95D827357AFAL; 0x18A6A990C8B35EBDL;
      0xCCCB7005C6B9C28DL; 0x3BDBB92C43B17F26L; 0xAA70B5B4F89695A2L; 0xE94C39A54A98307FL;
      0xB7A0B174CFF6F36EL; 0xD4DBA84729AF48ADL; 0x2E18BC1AD9704A68L; 0x2DE0966DAF2F8B1CL;
      0xB9C11D5B1E43A07EL; 0x64972D68DEE33360L; 0x94628D38D0C20584L; 0xDBC0D2B6AB90A559L;
      0xD2733C4335C6A72FL; 0x7E75D99D94A70F4DL; 0x6CED1983376FA72BL; 0x97FCAACBF030BC24L;
      0x7B77497B32503B12L; 0x8547EDDFB81CCB94L; 0x79999CDFF70902CBL; 0xCFFE1939438E9B24L;
      0x829626E3892D95D7L; 0x92FAE24291F2B3F1L; 0x63E22C147B9C3403L; 0xC678B6D860284A1CL;
      0x5873888850659AE7L; 0x0981DCD296A8736DL; 0x9F65789A6509A440L; 0x9FF38FED72E9052FL;
      0xE479EE5B9930578CL; 0xE7F28ECD2D49EECDL; 0x56C074A581EA17FEL; 0x5544F7D774B14AEFL;
      0x7B3F0195FC6F290FL; 0x12153635B2C0CF57L; 0x7F5126DBBA5E0CA7L; 0x7A76956C3EAFB413L;
      0x3D5774A11D31AB39L; 0x8A1B083821F40CB4L; 0x7B4A38E32537DF62L; 0x950113646D1D6E03L;
      0x4DA8979A0041E8A9L; 0x3BC36E078F7515D7L; 0x5D0A12F27AD310D1L; 0x7F9D1A2E1EBE1327L;
      0xDA3A361B1C5157B1L; 0xDCDD7D20903D0C25L; 0x36833336D068F707L; 0xCE68341F79893389L;
      0xAB9090168DD05F34L; 0x43954B3252DC25E5L; 0xB438C2B67F98E5E9L; 0x10DCD78E3851A492L;
      0xDBC27AB5447822BFL; 0x9B3CDB65F82CA382L; 0xB67B7896167B4C84L; 0xBFCED1B0048EAC50L;
      0xA9119B60369FFEBDL; 0x1FFF7AC80904BF45L; 0xAC12FB171817EEE7L; 0xAF08DA9177DDA93DL;
      0x1B0CAB936E65C744L; 0xB559EB1D04E5E932L; 0xC37B45B3F8D6F2BAL; 0xC3A9DC228CAAC9E9L;
      0xF3B8B6675A6507FFL; 0x9FC477DE4ED681DAL; 0x67378D8ECCEF96CBL; 0x6DD856D94D259236L;
      0xA319CE15B0B4DB31L; 0x073973751F12DD5EL; 0x8A8E849EB32781A5L; 0xE1925C71285279F5L;
      0x74C04BF1790C0EFEL; 0x4DDA48153C94938AL; 0x9D266D6A1CC0542CL; 0x7440FB816508C4FEL;
      0x13328503DF48229FL; 0xD6BF7BAEE43CAC40L; 0x4838D65F6EF6748FL; 0x1E152328F3318DEAL;
      0x8F8419A348F296BFL; 0x72C8834A5957B511L; 0xD7A023A73260B45CL; 0x94EBC8ABCFB56DAEL;
      0x9FC10D0F989993E0L; 0xDE68A2355B93CAE6L; 0xA44CFE79AE538BBEL; 0x9D1D84FCCE371425L;
      0x51D2B1AB2DDFB636L; 0x2FD7E4B9E72CD38CL; 0x65CA5B96B7552210L; 0xDD69A0D8AB3B546DL;
      0x604D51B25FBF70E2L; 0x73AA8A564FB7AC9EL; 0x1A8C1E992B941148L; 0xAAC40A2703D9BEA0L;
      0x764DBEAE7FA4F3A6L; 0x1E99B96E70A9BE8BL; 0x2C5E9DEB57EF4743L; 0x3A938FEE32D29981L;
      0x26E6DB8FFDF5ADFEL; 0x469356C504EC9F9DL; 0xC8763C5B08D1908CL; 0x3F6C6AF859D80055L;
      0x7F7CC39420A3A545L; 0x9BFB227EBDF4C5CEL; 0x89039D79D6FC5C5CL; 0x8FE88B57305E2AB6L;
      0xA09E8C8C35AB96DEL; 0xFA7E393983325753L; 0xD6B6D0ECC617C699L; 0xDFEA21EA9E7557E3L;
      0xB67C1FA481680AF8L; 0xCA1E3785A9E724E5L; 0x1CFC8BED0D681639L; 0xD18D8549D140CAEAL;
      0x4ED0FE7E9DC91335L; 0xE4DBF0634473F5D2L; 0x1761F93A44D5AEFEL; 0x53898E4C3910DA55L;
      0x734DE8181F6EC39AL; 0x2680B122BAA28D97L; 0x298AF231C85BAFABL; 0x7983EED3740847D5L;
      0x66C1A2A1A60CD889L; 0x9E17E49642A3E4C1L; 0xEDB454E7BADC0805L; 0x50B704CAB602C329L;
      0x4CC317FB9CDDD023L; 0x66B4835D9EAFEA22L; 0x219B97E26FFC81BDL; 0x261E4E4C0A333A9DL;
      0x1FE2CCA76517DB90L; 0xD7504DFA8816EDBBL; 0xB9571FA04DC089C8L; 0x1DDC0325259B27DEL;
      0xCF3F4688801EB9AAL; 0xF4F5D05C10CAB243L; 0x38B6525C21A42B0EL; 0x36F60E2BA4FA6800L;
      0xEB3593803173E0CEL; 0x9C4CD6257C5A3603L; 0xAF0C317D32ADAA8AL; 0x258E5A80C7204C4BL;
      0x8B889D624D44885DL; 0xF4D14597E660F855L; 0xD4347F66EC8941C3L; 0xE699ED85B0DFB40DL;
      0x2472F6207C2D0484L; 0xC2A1E7B5B459AEB5L; 0xAB4F6451CC1D45ECL; 0x63767572AE3D6174L;
      0xA59E0BD101731A28L; 0x116D0016CB948F09L; 0x2CF9C8CA052F6E9FL; 0x0B090A7560A968E3L;
      0xABEEDDB2DDE06FF1L; 0x58EFC10B06A2068DL; 0xC6E57A78FBD986E0L; 0x2EAB8CA63CE802D7L;
      0x14A195640116F336L; 0x7C0828DD624EC390L; 0xD74BBE77E6116AC7L; 0x804456AF10F5FB53L;
      0xEBE9EA2ADF4321C7L; 0x03219A39EE587A30L; 0x49787FEF17AF9924L; 0xA1E9300CD8520548L;
      0x5B45E522E4B1B4EFL; 0xB49C3B3995091A36L; 0xD4490AD526F14431L; 0x12A8F216AF9418C2L;
      0x001F837CC7350524L; 0x1877B51E57A764D5L; 0xA2853B80F17F58EEL; 0x993E1DE72D36D310L;
      0xB3598080CE64A656L; 0x252F59CF0D9F04BBL; 0xD23C8E176D113600L; 0x1BDA0492E7E4586EL;
      0x21E0BD5026C619BFL; 0x3B097ADAF088F94EL; 0x8D14DEDB30BE846EL; 0xF95CFFA23AF5F6F4L;
      0x3871700761B3F743L; 0xCA672B91E9E4FA16L; 0x64C8E531BFF53B55L; 0x241260ED4AD1E87DL;
      0x106C09B972D2E822L; 0x7FBA195410E5CA30L; 0x7884D9BC6CB569D8L; 0x0647DFEDCD894A29L;
      0x63573FF03E224774L; 0x4FC8E9560F91B123L; 0x1DB956E450275779L; 0xB8D91274B9E9D4FBL;
      0xA2EBEE47E2FBFCE1L; 0xD9F1F30CCD97FB09L; 0xEFED53D75FD64E6BL; 0x2E6D02C36017F67FL;
      0xA9AA4D20DB084E9BL; 0xB64BE8D8B25396C1L; 0x70CB6AF7C2D5BCF0L; 0x98F076A4F7A2322EL;
      0xBF84470805E69B5FL; 0x94C3251F06F90CF3L; 0x3E003E616A6591E9L; 0xB925A6CD0421AFF3L;
      0x61BDD1307C66E300L; 0xBF8D5108E27E0D48L; 0x240AB57A8B888B20L; 0xFC87614BAF287E07L;
      0xEF02CDD06FFDB432L; 0xA1082C0466DF6C0AL; 0x8215E577001332C8L; 0xD39BB9C3A48DB6CFL;
      0x2738259634305C14L; 0x61CF4F94C97DF93DL; 0x1B6BACA2AE4E125BL; 0x758F450C88572E0BL;
      0x959F587D507A8359L; 0xB063E962E045F54DL; 0x60E8ED72C0DFF5D1L; 0x7B64978555326F9FL;
      0xFD080D236DA814BAL; 0x8C90FD9B083F4558L; 0x106F72FE81E2C590L; 0x7976033A39F7D952L;
      0xA4EC0132764CA04BL; 0x733EA705FAE4FA77L; 0xB4D8F77BC3E56167L; 0x9E21F4F903B33FD9L;
      0x9D765E419FB69F6DL; 0xD30C088BA61EA5EFL; 0x5D94337FBFAF7F5BL; 0x1A4E4822EB4D7A59L;
      0x6FFE73E81B637FB3L; 0xDDF957BC36D8B9CAL; 0x64D0E29EEA8838B3L; 0x08DD9BDFD96B9F63L;
      0x087E79E5A57D1D13L; 0xE328E230E3E2B3FBL; 0x1C2559E30F0946BEL; 0x720BF5F26F4D2EAAL;
      0xB0774D261CC609DBL; 0x443F64EC5A371195L; 0x4112CF68649A260EL; 0xD813F2FAB7F5C5CAL;
      0x660D3257380841EEL; 0x59AC2C7873F910A3L; 0xE846963877671A17L; 0x93B633ABFA3469F8L;
      0xC0C0F5A60EF4CDCFL; 0xCAF21ECD4377B28CL; 0x57277707199B8175L; 0x506C11B9D90E8B1DL;
      0xD83CC2687A19255FL; 0x4A29C6465A314CD1L; 0xED2DF21216235097L; 0xB5635C95FF7296E2L;
      0x22AF003AB672E811L; 0x52E762596BF68235L; 0x9AEBA33AC6ECC6B0L; 0x944F6DE09134DFB6L;
      0x6C47BEC883A7DE39L; 0x6AD047C430A12104L; 0xA5B1CFDBA0AB4067L; 0x7C45D833AFF07862L;
      0x5092EF950A16DA0BL; 0x9338E69C052B8E7BL; 0x455A4B4CFE30E3F5L; 0x6B02E63195AD0CF8L;
      0x6B17B224BAD6BF27L; 0xD1E0CCD25BB9C169L; 0xDE0C89A556B9AE70L; 0x50065E535A213CF6L;
      0x9C1169FA2777B874L; 0x78EDEFD694AF1EEDL; 0x6DC93D9526A50E68L; 0xEE97F453F06791EDL;
      0x32AB0EDB696703D3L; 0x3A6853C7E70757A7L; 0x31865CED6120F37DL; 0x67FEF95D92607890L;
      0x1F2B1D1F15F6DC9CL; 0xB69E38A8965C6B65L; 0xAA9119FF184CCCF4L; 0xF43C732873F24C13L;
      0xFB4A3D794A9A80D2L; 0x3550C2321FD6109CL; 0x371F77E76BB8417EL; 0x6BFA9AAE5EC05779L;
      0xCD04F3FF001A4778L; 0xE3273522064480CAL; 0x9F91508BFFCFC14AL; 0x049A7F41061A9E60L;
      0xFCB6BE43A9F2FE9BL; 0x08DE8A1C7797DA9BL; 0x8F9887E6078735A1L; 0xB5B4071DBFC73A66L;
      0x230E343DFBA08D33L; 0x43ED7F5A0FAE657DL; 0x3A88A0FBBCB05C63L; 0x21874B8B4D2DBC4FL;
      0x1BDEA12E35F6A8C9L; 0x53C065C6C8E63528L; 0xE34A1D250E7A8D6BL; 0xD6B04D3B7651DD7EL;
      0x5E90277E7CB39E2DL; 0x2C046F22062DC67DL; 0xB10BB459132D0A26L; 0x3FA9DDFB67E2F199L;
      0x0E09B88E1914F7AFL; 0x10E8B35AF3EEAB37L; 0x9EEDECA8E272B933L; 0xD4C718BC4AE8AE5FL;
      0x81536D601170FC20L; 0x91B534F885818A06L; 0xEC8177F83F900978L; 0x190E714FADA5156EL;
      0xB592BF39B0364963L; 0x89C350C893AE7DC1L; 0xAC042E70F8B383F2L; 0xB49B52E587A1EE60L;
      0xFB152FE3FF26DA89L; 0x3E666E6F69AE2C15L; 0x3B544EBE544C19F9L; 0xE805A1E290CF2456L;
      0x24B33C9D7ED25117L; 0xE74733427B72F0C1L; 0x0A804D18B7097475L; 0x57E3306D881EDB4FL;
      0x4AE7D6A36EB5DBCBL; 0x2D8D5432157064C8L; 0xD1E649DE1E7F268BL; 0x8A328A1CEDFE552CL;
      0x07A3AEC79624C7DAL; 0x84547DDC3E203C94L; 0x990A98FD5071D263L; 0x1A4FF12616EEFC89L;
      0xF6F7FD1431714200L; 0x30C05B1BA332F41CL; 0x8D2636B81555A786L; 0x46C9FEB55D120902L;
      0xCCEC0A73B49C9921L; 0x4E9D2827355FC492L; 0x19EBB029435DCB0FL; 0x4659D2B743848A2CL;
      0x963EF2C96B33BE31L; 0x74F85198B05A2E7DL; 0x5A0F544DD2B1FB18L; 0x03727073C2E134B1L;
      0xC7F6AA2DE59AEA61L; 0x352787BAA0D7C22FL; 0x9853EAB63B5E0B35L; 0xABBDCDD7ED5C0860L;
      0xCF05DAF5AC8D77B0L; 0x49CAD48CEBF4A71EL; 0x7A4C10EC2158C4A6L; 0xD9E92AA246BF719EL;
      0x13AE978D09FE5557L; 0x730499AF921549FFL; 0x4E4B705B92903BA4L; 0xFF577222C14F0A3AL;
      0x55B6344CF97AAFAEL; 0xB862225B055B6960L; 0xCAC09AFBDDD2CDB4L; 0xDAF8E9829FE96B5FL;
      0xB5FDFC5D3132C498L; 0x310CB380DB6F7503L; 0xE87FBB46217A360EL; 0x2102AE466EBB1148L;
      0xF8549E1A3AA5E00DL; 0x07A69AFDCC42261AL; 0xC4C118BFE78FEAAEL; 0xF9F4892ED96BD438L;
      0x1AF3DBE25D8F45DAL; 0xF5B4B0B0D2DEEEB4L; 0x962ACEEFA82E1C84L; 0x046E3ECAAF453CE9L;
      0xF05D129681949A4CL; 0x964781CE734B3C84L; 0x9C2ED44081CE5FBDL; 0x522E23F3925E319EL;
      0x177E00F9FC32F791L; 0x2BC60A63A6F3B3F2L; 0x222BBFAE61725606L; 0x486289DDCC3D6780L;
      0x7DC7785B8EFDFC80L; 0x8AF38731C02BA980L; 0x1FAB64EA29A2DDF7L; 0xE4D9429322CD065AL;
      0x9DA058C67844F20CL; 0x24C0E332B70019B0L; 0x233003B5A6CFE6ADL; 0xD586BD01C5C217F6L;
      0x5E5637885F29BC2BL; 0x7EBA726D8C94094BL; 0x0A56A5F0BFE39272L; 0xD79476A84EE20D06L;
      0x9E4C1269BAA4BF37L; 0x17EFEE45B0DEE640L; 0x1D95B0A5FCF90BC6L; 0x93CBE0B699C2585DL;
      0x65FA4F227A2B6D79L; 0xD5F9E858292504D5L; 0xC2B5A03F71471A6FL; 0x59300222B4561E00L;
      0xCE2F8642CA0712DCL; 0x7CA9723FBB2E8988L; 0x2785338347F2BA08L; 0xC61BB3A141E50E8CL;
      0x150F361DAB9DEC26L; 0x9F6A419D382595F4L; 0x64A53DC924FE7AC9L; 0x142DE49FFF7A7C3DL;
      0x0C335248857FA9E7L; 0x0A9C32D5EAE45305L; 0xE6C42178C4BBB92EL; 0x71F1CE2490D20B07L;
      0xF1BCC3D275AFE51AL; 0xE728E8C83C334074L; 0x96FBF83A12884624L; 0x81A1549FD6573DA5L;
      0x5FA7867CAF35E149L; 0x56986E2EF3ED091BL; 0x917F1DD5F8886C61L; 0xD20D8C88C8FFE65FL;
    |]

    let castle_keys = [|
      0x31D71DCE64B2C310L; 0xF165B587DF898190L; 0xA57E6339DD2CF3A0L; 0x1EF6E6DBB1961EC9L;
    |]

    let en_passant_keys = [|
      0x70CC73D90BC26E24L; 0xE21A6B35DF0C3AD7L; 0x003A93D8B2806962L; 0x1C99DED33CB890A1L;
      0xCF3145DE0ADD4289L; 0xD0E4427A5514FB72L; 0x77C621CC9FB3A483L; 0x67A34DAC4356550BL;
    |]

    let white_to_move_key = 0xF8D626AAAF278509L

    let[@inline] piece_key c k sq =
      let c = Piece.Color.(to_int @@ opposite c) in
      let k = Piece.Kind.to_int k in
      let i = (k lsl 1) lor c in
      Array.unsafe_get piece_keys (Square.to_int sq + i * Square.count)

    let[@inline] castle_key c s =
      let i = (Piece.Color.to_int c lsl 1) lor (Cr.Side.to_int s) in
      Array.unsafe_get castle_keys i

    let[@inline] en_passant_key file = Array.unsafe_get en_passant_keys file
  end

  (* Helpers for updating the hash. *)
  module Update = struct
    let[@inline] flip h x = Int64.(h lxor x)
    let[@inline] piece c k sq h = flip h @@ Keys.piece_key c k sq
    let[@inline] castle c s h = flip h @@ Keys.castle_key c s
    let[@inline] en_passant_file file h = flip h @@ Keys.en_passant_key file
    let[@inline] active_player h = flip h Keys.white_to_move_key

    let[@inline] en_passant ep h =
      Uopt.value_map ep ~default:h ~f:(fun ep ->
          en_passant_file (Square.file ep) h)

    let[@inline] castle_test cr c s h =
      if Cr.mem cr c s then castle c s h else h
  end

  (* Get the hash of a position. *)
  let of_position =
    let module H = Monad.State.Make(Int64)(Monad.Ident) in
    let open H.Syntax in
    fun pos -> Monad.State.exec begin
        (* Piece placement.. *)
        collect_all pos |> H.List.iter ~f:(fun (sq, p) ->
            let c, k = Piece.decomp p in
            H.update @@ Update.piece c k sq) >>= fun () ->
        (* Castling rights *)
        H.update @@ Update.castle_test pos.castle White Kingside  >>= fun () ->
        H.update @@ Update.castle_test pos.castle White Queenside >>= fun () ->
        H.update @@ Update.castle_test pos.castle Black Kingside  >>= fun () ->
        H.update @@ Update.castle_test pos.castle Black Queenside >>= fun () ->
        (* En passant *)
        H.update @@ Update.en_passant pos.en_passant >>= fun () ->
        (* White to move. *)
        match pos.active with
        | White -> H.update @@ Update.active_player
        | Black -> H.return ()
      end 0L

  let pawn_structure pos = Fn.flip Monad.State.exec 0L @@
    let module H = Monad.State.Make(Int64)(Monad.Ident) in
    let open H.Syntax in
    collect_kind pos Pawn |> H.List.iter ~f:(fun (sq, c) ->
        H.update @@ Update.piece c Pawn sq)
end

let same_hash pos1 pos2 = Int64.(pos1.hash = pos2.hash)

(* Attack patterns. *)

module Attacks = struct
  (* Useful when excluding squares that are occupied by our color. *)
  let[@inline] ignore_color pos c b = Bb.(b - board_of_color pos c)

  (* Generate for a particular color and kind *)
  let[@inline] gen ?(ignore_same = true) pos c k f =
    let open Bb.Syntax in
    Piece.create c k |> board_of_piece pos |> Bb.fold
      ~init:Bb.empty ~f:(fun acc sq -> acc + f sq) |>
    fun b -> if ignore_same then ignore_color pos c b else b

  let[@inline] pawn ?(ignore_same = true) pos c =
    gen pos c Pawn ~ignore_same @@ fun sq -> Pre.pawn_capture sq c

  let[@inline] knight ?(ignore_same = true) pos c =
    gen pos c Knight Pre.knight ~ignore_same

  (* Get the occupied squares for the board. Kingside_danger` indicates that the
     king of the opposite color should be ignored, so that sliding attacks
     can "see through" the inactive king. This is useful when the king is blocking
     the attack of a sliding piece. *)
  let[@inline] occupied pos c king_danger =
    let open Bb.Syntax in
    if king_danger then
      let p = Piece.(create (Color.opposite c) King) in
      all_board pos - board_of_piece pos p
    else all_board pos

  let[@inline] bishop ?(ignore_same = true) ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Bishop ~ignore_same @@ fun sq -> Pre.bishop sq occupied

  let[@inline] rook ?(ignore_same = true) ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Rook ~ignore_same @@ fun sq -> Pre.rook sq occupied

  let[@inline] queen ?(ignore_same = true) ?(king_danger = false) pos c =
    let occupied = occupied pos c king_danger in
    gen pos c Queen ~ignore_same @@ fun sq -> Pre.queen sq occupied

  let[@inline] king ?(ignore_same = true) pos c =
    gen pos c King Pre.king ~ignore_same

  let[@inline] pre_of_kind sq occupied c = function
    | Piece.Pawn   -> Pre.pawn_capture sq c
    | Piece.Knight -> Pre.knight sq
    | Piece.Bishop -> Pre.bishop sq occupied
    | Piece.Rook   -> Pre.rook sq occupied
    | Piece.Queen  -> Pre.queen sq occupied
    | Piece.King   -> Pre.king sq

  let[@inline] aux ?(ignore_same = true) ?(king_danger = false) pos c ~f =
    let open Bb.Syntax in
    let occupied = occupied pos c king_danger in
    collect_color pos c |> List.fold ~init:Bb.empty ~f:(fun acc (sq, k) ->
        if f k then acc + pre_of_kind sq occupied c k else acc) |>
    fun b -> if ignore_same then ignore_color pos c b else b

  let[@inline] all ?(ignore_same = true) ?(king_danger = false) pos c =
    aux pos c ~ignore_same ~king_danger ~f:(fun _ -> true)

  let[@inline] sliding ?(ignore_same = true) ?(king_danger = false) pos c =
    aux pos c ~ignore_same ~king_danger ~f:(function
        | Piece.(Bishop | Rook | Queen) -> true
        | _ -> false)

  let[@inline] non_sliding ?(ignore_same = true) pos c =
    aux pos c ~ignore_same ~f:(function
        | Piece.(Bishop | Rook | Queen) -> false
        | _ -> true)
end

let in_check pos =
  let active_board = active_board pos in
  let attacks = Attacks.all pos (inactive pos) ~ignore_same:true in
  Bb.((active_board & pos.king & attacks) <> empty)

let is_insufficient_material pos = let open Bb in
  let active = active_board pos in
  let inactive = inactive_board pos in
  let occupied = active + inactive in
  let kb = pos.king + pos.bishop in
  let kn = pos.king + pos.knight in
  (* Only kings are left. *)
  pos.king = occupied ||
  (* King + knight/bishop vs king *)
  (kb = occupied && Int.equal 3 @@ count kb) ||
  (kn = occupied && Int.equal 3 @@ count kn) ||
  (* King + bishop vs king + bishop of the same color square. *)
  ((kb & active) = active &&
   (kb & inactive) = inactive &&
   Int.equal 2 @@ count (kb & active) &&
   Int.equal 2 @@ count (kb & inactive) &&
   Piece.Color.equal
     (Square.color @@ Bb.first_set_exn (pos.bishop & active))
     (Square.color @@ Bb.first_set_exn (pos.bishop & inactive)))

(* Relevant info about the position for generating moves, as well as performing
   sanity checks. *)

module Analysis = struct
  module T = struct
    type t = {
      pos : T.t;
      king_sq : Square.t;
      en_passant_pawn : Square.t Uopt.t;
      occupied : Bb.t;
      active_board : Bb.t;
      inactive_board : Bb.t;
      inactive_attacks : Bb.t;
      pinners : Bb.t array;
      num_checkers : int;
      check_mask : Bb.t;
      en_passant_check_mask : Bb.t;
      inactive_sliders : (Square.t * Piece.kind) list;
    } [@@deriving fields]
  end

  (* Attacks of all piece kinds, starting from the king, intersected with
     the squares occupied by the corresponding inactive pieces. *)
  let[@inline] checkers pos ~king_sq ~inactive_board ~occupied =
    let open Bb.Syntax in
    let p = Pre.pawn_capture king_sq pos.active & pos.pawn in
    let n = Pre.knight king_sq & pos.knight in
    let bishop = Pre.bishop king_sq occupied in
    let rook = Pre.rook king_sq occupied in
    let bq = bishop & (pos.bishop + pos.queen) in
    let rq = rook & (pos.rook + pos.queen) in
    let k = Pre.king king_sq & pos.king in
    (p + n + bq + rq + k) & inactive_board

  (* For each inactive sliding piece, calculate its attack set. Then,
     intersect it with the same attack set from our king's square.
     Then, intersect with the squares between the sliding piece and our
     king. Any of our pieces that are in this intersection are thus
     pinned. *)
  let[@inline] pinners ~active_board ~king_sq ~inactive_sliders ~occupied =
    let open Bb in
    let bishop  = Pre.bishop king_sq occupied in
    let rook    = Pre.rook   king_sq occupied in
    let queen   = Pre.queen  king_sq occupied in
    let mask    = active_board -- king_sq in
    let pinners = Array.create ~len:Square.count empty in
    List.iter inactive_sliders ~f:(fun (sq, k) ->
        let checker, king = match k with
          | Piece.Bishop -> Pre.bishop sq occupied, bishop
          | Piece.Rook   -> Pre.rook   sq occupied, rook
          | Piece.Queen  -> Pre.queen  sq occupied, queen
          | _ -> empty, empty in
        let b = checker & king & mask & Pre.between king_sq sq in
        if b <> empty then
          let i = Square.to_int @@ first_set_exn b in
          let b = Array.unsafe_get pinners i ++ sq in
          Array.unsafe_set pinners i b);
    pinners

  (* Generate the masks which may restrict movement in the event of a check. *)
  let[@inline] checks pos ~en_passant_pawn:pw ~num_checkers ~checkers ~king_sq =
    if num_checkers = 1 then
      (* Test if the checker is a sliding piece. If so, then we can try to
         block the attack. Otherwise, they may only be captured. *)
      let open Bb.Syntax in
      let sq = Bb.first_set_exn checkers in
      match which_kind_exn pos sq with
      | Bishop | Rook | Queen -> checkers + Pre.between king_sq sq, Bb.empty
      | Pawn when Uopt.is_some pw ->
        (* Edge case for being able to get out of check via en passant
           capture. *)
        let ep, pw = Uopt.(unsafe_value pos.en_passant, unsafe_value pw) in
        if Square.(sq = pw) then checkers, !!ep else checkers, Bb.empty
      |  _ -> checkers, Bb.empty
    else Bb.(full, empty)

  (* Populate info needed for generating legal moves. *)
  let[@inline] create pos =
    (* First, find our king. *)
    let king_sq = Bb.(first_set_exn (pos.king & active_board pos)) in
    (* Square of the en passant pawn. *)
    let en_passant_pawn = en_passant_pawn_uopt pos in
    (* Most general info. *)
    let inactive = inactive pos in
    let occupied = all_board pos in
    let active_board = active_board pos in
    let inactive_board = inactive_board pos in
    let inactive_pieces = collect_color pos inactive in
    (* We're considering attacked squares only for king moves. These squares
       should include inactive pieces which may block an inactive attack, since
       it would be illegal for the king to attack those squares. *)
    let inactive_attacks = let open Bb in 
      let occupied = occupied -- king_sq in
      List.fold inactive_pieces ~init:empty ~f:(fun acc (sq, k) ->
          acc + Attacks.pre_of_kind sq occupied inactive k) in
    (* Sliding pieces will be used to calculate pins. *)
    let inactive_sliders =
      List.filter inactive_pieces ~f:(fun (_, k) -> Piece.Kind.is_sliding k) in
    (* Pinned pieces. *)
    let pinners = pinners ~active_board ~king_sq ~inactive_sliders ~occupied in
    (* Pieces checking our king. *)
    let checkers = checkers pos ~king_sq ~inactive_board ~occupied in
    (* Number of checkers is important for how we can decide to get out of
       check. *)
    let num_checkers = Bb.count checkers in
    (* Masks which will may allow us to escape check. *)
    let check_mask, en_passant_check_mask =
      checks pos ~en_passant_pawn ~num_checkers ~checkers ~king_sq in
    (* Construct the analyzed position. *)
    T.Fields.create
      ~pos ~king_sq ~en_passant_pawn ~occupied ~active_board
      ~inactive_board ~inactive_attacks ~pinners ~num_checkers
      ~check_mask ~en_passant_check_mask ~inactive_sliders

  include T
end

(* Validation *)

module Valid = struct
  module Error = struct
    type t =
      | Empty_board
      | Full_board
      | Invalid_number_of_kings of Piece.color * int
      | Kings_not_separated
      | Inactive_in_check of Piece.color
      | Invalid_number_of_checkers of Piece.color * int
      | Invalid_two_checkers of Piece.color * Piece.kind * Piece.kind
      | Invalid_number_of_pawns of Piece.color * int
      | Pawns_in_back_rank of Piece.color
      | Missing_pawn_en_passant of Piece.color
      | Invalid_en_passant_square of Square.t
      | Invalid_extra_pieces of Piece.color * int
      | Invalid_number_of_pieces of Piece.color * int
      | Invalid_castling_rights of Piece.color * Piece.kind
      | En_passant_wrong_halfmove
      | Invalid_halfmove
      | Invalid_fullmove

    let to_string = function
      | Empty_board -> "Board is empty"
      | Full_board -> "Board is full"
      | Invalid_number_of_kings (c, n) ->
        sprintf "Invalid number of %s kings (%d)"
          (Piece.Color.to_string_hum c) n
      | Kings_not_separated ->
        sprintf "Kings must be separated by at least one square"
      | Inactive_in_check c ->
        sprintf "Inactive player %s is in check" @@
        Piece.Color.to_string_hum c
      | Invalid_number_of_checkers (c, n) ->
        sprintf "Player %s has %d checkers, max is two"
          (Piece.Color.to_string_hum c) n
      | Invalid_two_checkers (c, k1, k2) ->
        sprintf "Player %s has invalid two checkers: %s and %s"
          (Piece.Color.to_string_hum c)
          (Piece.Kind.to_string_hum k1)
          (Piece.Kind.to_string_hum k2)
      | Invalid_number_of_pawns (c, n) ->
        sprintf "Invalid number of %s pawns (%d)"
          (Piece.Color.to_string_hum c) n
      | Pawns_in_back_rank c ->
        sprintf "Player %s has pawns in the back rank" @@
        Piece.Color.to_string_hum c
      | Missing_pawn_en_passant c ->
        sprintf "Missing %s pawn in front of en passant square" @@
        Piece.Color.to_string_hum c
      | Invalid_en_passant_square sq ->
        Format.asprintf "Invalid en passant square %a" Square.pp sq
      | Invalid_extra_pieces (c, n) ->
        sprintf "Invalid number of extra %s pieces (%d)"
          (Piece.Color.to_string_hum c) n
      | Invalid_number_of_pieces (c, n) ->
        sprintf "Invalid number of %s pieces (%d)"
          (Piece.Color.to_string_hum c) n
      | Invalid_castling_rights (c, k) ->
        sprintf "Invalid castling rights, %s %s moved"
          (Piece.Color.to_string_hum c)
          (Piece.Kind.to_string_hum k)
      | En_passant_wrong_halfmove ->
        sprintf "En passant square is set, but halfmove clock is not zero"
      | Invalid_halfmove -> sprintf "Invalid halfmove clock"
      | Invalid_fullmove -> sprintf "Invalid fullmove clock"
  end

  type error = Error.t

  module E = Monad.Result.Make(Error)(Monad.Ident)

  open E.Syntax

  module Trivial = struct
    let check_empty pos =
      if Bb.(all_board pos = empty) then E.fail Empty_board else E.return ()

    let check_full pos =
      if Bb.(all_board pos = full) then E.fail Empty_board else E.return ()

    let check_sixteen pos c =
      let n = Bb.count @@ board_of_color pos c in
      if n > 16 then E.fail @@ Invalid_number_of_pieces (c, n)
      else E.return ()

    let go pos =
      check_empty pos >>= fun () ->
      check_full pos >>= fun () ->
      check_sixteen pos White >>= fun () ->
      check_sixteen pos Black
  end

  module King = struct
    let check_count pos =
      let wk = Bb.(count (pos.white & pos.king)) in
      if wk <> 1 then E.fail @@ Invalid_number_of_kings (White, wk)
      else
        let bk = Bb.(count (pos.black & pos.king)) in
        if bk <> 1 then E.fail @@ Invalid_number_of_kings (Black, bk)
        else E.return ()

    let check_sep pos =
      let k1 = Bb.(first_set_exn (pos.white & pos.king)) in
      let k2 = Bb.(first_set_exn (pos.black & pos.king)) in
      if Square.chebyshev k1 k2 <= 1
      then E.fail Kings_not_separated
      else E.return ()

    let go pos =
      check_count pos >>= fun () ->
      check_sep pos
  end

  module Checks = struct
    let check_inactive_in_check pos =
      let b = inactive_board pos in
      let attacks = Attacks.all pos pos.active ~ignore_same:true in
      if Bb.((b & pos.king & attacks) <> empty)
      then E.fail @@ Inactive_in_check (inactive pos)
      else E.return ()

    let check_checkers pos =
      let active_board = active_board pos in
      let king_sq = Bb.(first_set_exn (active_board & pos.king)) in
      let inactive_board = inactive_board pos in
      let occupied = Bb.(active_board + inactive_board) in
      let checkers = Analysis.checkers pos ~king_sq ~inactive_board ~occupied in
      let num_checkers = Bb.count checkers in
      if num_checkers >= 3
      then E.fail @@ Invalid_number_of_checkers (pos.active, num_checkers)
      else
        let checkers = Bb.fold checkers ~init:[] ~f:(fun acc sq ->
            which_kind_exn pos sq :: acc) in
        match checkers with
        | [Pawn; Pawn] ->
          E.fail @@ Invalid_two_checkers (pos.active, Pawn, Pawn)
        | [Pawn; Knight] | [Knight; Pawn] ->
          E.fail @@ Invalid_two_checkers (pos.active, Pawn, Knight)
        | [Pawn; Bishop] | [Bishop; Pawn] ->
          E.fail @@ Invalid_two_checkers (pos.active, Pawn, Bishop)
        | [Knight; Knight] ->
          E.fail @@ Invalid_two_checkers (pos.active, Knight, Knight)
        | [Bishop; Bishop] ->
          E.fail @@ Invalid_two_checkers (pos.active, Bishop, Bishop)
        | _ -> E.return ()

    let go pos = 
      check_inactive_in_check pos >>= fun () ->
      check_checkers pos
  end

  module Pawn = struct
    let check_count pos =
      let wp = Bb.(count (pos.white & pos.pawn)) in
      if wp > 8 then E.fail @@ Invalid_number_of_pawns (White, wp)
      else
        let bp = Bb.(count (pos.black & pos.pawn)) in
        if bp > 8 then E.fail @@ Invalid_number_of_pawns (Black, bp)
        else E.return ()

    let check_back_rank pos =
      let mask = Bb.(rank_1 + rank_8) in
      if Bb.((pos.white & pos.pawn & mask) <> empty)
      then E.fail @@ Pawns_in_back_rank White
      else if Bb.((pos.black & pos.pawn & mask) <> empty)
      then E.fail @@ Pawns_in_back_rank Black
      else E.return ()

    let check_en_passant pos =
      let ep = pos.en_passant in
      if Uopt.is_none ep then E.return ()
      else
        let ep = Uopt.unsafe_value ep in
        let rank, file = Square.decomp ep in
        if rank = Square.Rank.three then
          let sq = Square.create_exn ~rank:(succ rank) ~file in
          let p = piece_at_square_uopt pos sq in
          if Uopt.is_none p then E.fail @@ Missing_pawn_en_passant White
          else
            let p = Uopt.unsafe_value p in
            if Piece.(is_white p && is_pawn p) then E.return ()
            else E.fail @@ Missing_pawn_en_passant White
        else if rank = Square.Rank.six then
          let sq = Square.create_exn ~rank:(pred rank) ~file in
          let p = piece_at_square_uopt pos sq in
          if Uopt.is_none p then E.fail @@ Missing_pawn_en_passant Black
          else
            let p = Uopt.unsafe_value p in
            if Piece.(is_black p && is_pawn p) then E.return ()
            else E.fail @@ Missing_pawn_en_passant Black
        else E.fail @@ Invalid_en_passant_square ep

    let check_promotions pos c b =
      let num_pawn   = Bb.(count (pos.pawn & b)) in
      let num_knight = Bb.(count (pos.knight & b)) in
      let num_bishop = Bb.(count (pos.bishop & b)) in
      let num_rook   = Bb.(count (pos.rook & b)) in
      let num_queen  = Bb.(count (pos.queen & b)) in
      let extra =
        max 0 (num_knight - 2) +
        max 0 (num_bishop - 2) +
        max 0 (num_rook   - 2) +
        max 0 (num_queen  - 1) in
      if extra > (8 - num_pawn) then E.fail @@ Invalid_extra_pieces (c, extra)
      else E.return ()

    let go pos =
      check_count pos >>= fun () ->
      check_back_rank pos >>= fun () ->
      check_en_passant pos >>= fun () ->
      check_promotions pos White pos.white >>= fun () ->
      check_promotions pos Black pos.black
  end

  module Castling = struct
    let check_king_moved pos c b =
      if Cr.(mem pos.castle c Kingside || mem pos.castle c Queenside) then
        let sq = match c with
          | White -> Square.e1
          | Black -> Square.e8 in
        if Bb.(sq @ (b & pos.king)) then E.return ()
        else E.fail @@ Invalid_castling_rights (c, King)
      else E.return ()

    let check_rook_moved pos c b s sq =
      if Cr.mem pos.castle c s then
        if Bb.(sq @ (b & pos.rook)) then E.return ()
        else E.fail @@ Invalid_castling_rights (c, Rook)
      else E.return ()

    let go pos =
      check_king_moved pos White pos.white >>= fun () ->
      check_king_moved pos Black pos.black >>= fun () ->
      check_rook_moved pos White pos.white Kingside  Square.h1 >>= fun () ->
      check_rook_moved pos White pos.white Queenside Square.a1 >>= fun () ->
      check_rook_moved pos Black pos.black Kingside  Square.h8 >>= fun () ->
      check_rook_moved pos Black pos.black Queenside Square.a8
  end

  module Half_and_fullmove = struct
    let check_en_passant pos =
      if Uopt.is_some pos.en_passant && pos.halfmove <> 0
      then E.fail En_passant_wrong_halfmove
      else E.return ()

    let check_both pos =
      if pos.halfmove >
         ((pos.fullmove - 1) * 2) + (Piece.Color.to_int pos.active)
      then E.fail Invalid_halfmove
      else if pos.halfmove < 0 then E.fail Invalid_halfmove
      else if pos.fullmove < 1 then E.fail Invalid_fullmove
      else E.return ()

    let go pos =
      check_en_passant pos >>= fun () ->
      check_both pos
  end

  let check pos =
    Trivial.go pos >>= fun () ->
    King.go pos >>= fun () ->
    Checks.go pos >>= fun () ->
    Pawn.go pos >>= fun () ->
    Castling.go pos >>= fun () ->
    Half_and_fullmove.go pos
end

(* FEN parsing/unparsing *)

module Fen = struct
  module Error = struct
    type t =
      | Invalid_number_of_ranks of int
      | Invalid_file_increment of int * Square.t
      | Rank_full of int
      | Invalid_piece_symbol of char * Square.t
      | Unspecified_squares of int * int
      | Invalid_active_color of string
      | Invalid_castling_rights of string
      | Invalid_en_passant of string
      | Invalid_halfmove of string
      | Invalid_fullmove of string
      | Invalid_position of Valid.error
      | Invalid_number_of_sections of int

    let to_string = function
      | Invalid_number_of_ranks n -> sprintf "Invalid number of ranks %d" n
      | Invalid_file_increment (n, sq) ->
        sprintf "Invalid file increment %d on square %s" n @@
        Square.to_string sq
      | Rank_full rank -> sprintf "Piece placement on full rank %d" (rank + 1)
      | Invalid_piece_symbol (sym, sq) ->
        sprintf "Invalid piece symbol '%c' placed at square %s" sym @@
        Square.to_string sq
      | Unspecified_squares (rank, n) ->
        sprintf "Rank %d has %d unspecified square(s)" (rank + 1) n
      | Invalid_active_color s -> sprintf "Invalid active color '%s'" s
      | Invalid_castling_rights s -> sprintf "Invalid castling rights '%s'" s
      | Invalid_en_passant s -> sprintf "Invalid en passant square '%s'" s
      | Invalid_halfmove s -> sprintf "Invalid halfmove clock '%s'" s
      | Invalid_fullmove s -> sprintf "Invalid fullmove clock '%s'" s
      | Invalid_position e ->
        sprintf "Invalid position; %s" @@ Valid.Error.to_string e
      | Invalid_number_of_sections n ->
        sprintf "Invalid number of sections %d" n
  end

  type error = Error.t

  module E = struct
    include Monad.Result.Make(Error)(Monad.Ident)

    module String = struct
      let fold =
        let rec loop s i acc ~f ~len =
          if i = len then return acc
          else
            f acc s.[i] >>= fun acc ->
            loop s (i + 1) acc ~f ~len in
        fun s ~init ~f -> loop s 0 init ~f ~len:(String.length s)
    end

    module List = struct
      include List

      let iteri =
        let rec loop i ~f = function
          | [] -> return ()
          | x :: xs ->
            f i x >>= fun () ->
            loop (i + 1) xs ~f in
        fun l ~f -> loop 0 l ~f
    end
  end

  open E.Syntax

  let start = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  let emit_placement buf pos =
    let adds = Buffer.add_string buf and addc = Buffer.add_char buf in
    let rec aux rank file skip =
      if rank < 0 then ()
      else if file > 7 then begin
        if skip > 0 then adds @@ Int.to_string skip;
        if rank > 0 then addc '/';
        aux (rank - 1) 0 0
      end else
        let p = piece_at_square_uopt pos @@ Square.create_exn ~rank ~file in
        if Uopt.is_none p then aux rank (file + 1) (skip + 1)
        else
          let p = Uopt.unsafe_value p in
          if skip > 0 then adds @@ Int.to_string skip;
          addc @@ Piece.to_fen p;
          aux rank (file + 1) 0 in
    aux 7 0 0

  let emit_active buf = function
    | Piece.White -> Buffer.add_char buf 'w'
    | Piece.Black -> Buffer.add_char buf 'b'

  let emit_castle buf cr = Buffer.add_string buf @@ Cr.to_string cr

  let emit_en_passant buf ep =
    Buffer.add_string buf @@
    Uopt.value_map ep ~default:"-" ~f:Square.to_string

  let to_string pos =
    let buf = Buffer.create 100 in
    let sep () = Buffer.add_char buf ' ' in
    emit_placement buf pos; sep ();
    emit_active buf @@ active pos; sep ();
    emit_castle buf @@ castle pos; sep ();
    emit_en_passant buf @@ pos.en_passant; sep ();
    Buffer.add_string buf @@ Int.to_string @@ halfmove pos; sep ();
    Buffer.add_string buf @@ Int.to_string @@ fullmove pos;
    Buffer.contents buf

  let parse_placement s =
    (* Split the ranks so we can parse them individually. *)
    begin match String.split s ~on:'/' with
      | [_; _; _; _; _; _; _; _] as ranks -> E.return @@ List.rev ranks
      | ranks -> E.fail @@ Invalid_number_of_ranks (List.length ranks)
    end >>= fun ranks ->
    (* Tables for our bitboards. *)
    let color_tbl = Array.create Bb.empty ~len:Piece.Color.count in
    let kind_tbl = Array.create Bb.empty ~len:Piece.Kind.count in
    (* The main entry to parsing the rank. *)
    let rec parse_rank rank file sym = match Char.get_digit sym with
      | Some inc -> skip_file rank file inc
      | None -> place_piece rank file sym
    (* Advance the file (we hit a numeric symbol). *)
    and skip_file rank file inc =
      let file' = file + inc in
      if file' > Square.File.count then E.fail @@
        Invalid_file_increment (inc, Square.create_exn ~rank ~file)
      else E.return file'
    (* Place a piece on the board (we hit an alphabetical symbol). *)
    and place_piece rank file sym =
      if file > Square.File.h then E.fail @@ Rank_full rank
      else
        let sq = Square.create_unsafe ~rank ~file in
        match Piece.of_fen sym with
        | Some p ->
          let c = Piece.(color p |> Color.to_int) in
          let k = Piece.(kind p |> Kind.to_int) in
          color_tbl.(c) <- Bb.(color_tbl.(c) ++ sq);
          kind_tbl.(k) <- Bb.(kind_tbl.(k) ++ sq);
          E.return (file + 1)
        | None -> E.fail @@ Invalid_piece_symbol (sym, sq) in
    (* Parse each rank individually. *)
    E.List.iteri ranks ~f:(fun rank s ->
        let init = Square.File.a and f = parse_rank rank in
        E.String.fold s ~init ~f >>= fun file ->
        let diff = Square.File.count - file in
        (* All eight squares of the rank must be specified. *)
        if diff <> 0 then E.fail @@ Unspecified_squares (rank, diff)
        else E.return ()) >>| fun () ->
    (* Return the bitboard tables. *)
    color_tbl, kind_tbl

  let parse_active = function
    | "w" -> E.return Piece.White
    | "b" -> E.return Piece.Black
    | s -> E.fail @@ Invalid_active_color s

  let parse_castle s = try E.return @@ Cr.of_string_exn s with
    | _ -> E.fail @@ Invalid_castling_rights s

  let parse_en_passant = function
    | "-" -> E.return Uopt.none
    | s -> try E.return @@ Uopt.some @@ Square.of_string_exn s with
      | _ -> E.fail @@ Invalid_en_passant s

  let parse_halfmove s = try
      E.return @@ Int.of_string s
    with _ -> E.fail @@ Invalid_halfmove s

  let parse_fullmove s = try
      E.return @@ Int.of_string s
    with _ -> E.fail @@ Invalid_fullmove s

  let validate_and_map pos = Error.(
      Valid.check pos |>
      Result.map_error ~f:(fun e -> Invalid_position e)) >>= fun () ->
    E.return pos

  let of_string ?(validate = true) s = match String.split s ~on:' ' with
    | [placement; active; castle; en_passant; halfmove; fullmove] ->
      parse_placement placement >>= fun (color_tbl, kind_tbl) ->
      let white  = color_tbl.(Piece.Color.white) in
      let black  = color_tbl.(Piece.Color.black) in
      let pawn   =  kind_tbl.(Piece.Kind.pawn)   in
      let knight =  kind_tbl.(Piece.Kind.knight) in
      let bishop =  kind_tbl.(Piece.Kind.bishop) in
      let rook   =  kind_tbl.(Piece.Kind.rook)   in
      let queen  =  kind_tbl.(Piece.Kind.queen)  in
      let king   =  kind_tbl.(Piece.Kind.king)   in
      parse_active active >>= fun active ->
      parse_castle castle >>= fun castle ->
      parse_en_passant en_passant >>= fun en_passant ->
      parse_halfmove halfmove >>= fun halfmove ->
      parse_fullmove fullmove >>= fun fullmove ->
      let pos = Fields.create
          ~white ~black ~pawn ~knight ~bishop ~rook ~queen ~king
          ~active ~castle ~en_passant ~halfmove ~fullmove
          ~hash:0L ~pawn_hash:0L in
      if not @@ has_en_passant_threat pos then set_en_passant pos Uopt.none;
      set_hash pos @@ Hash.of_position pos;
      set_pawn_hash pos @@ Hash.pawn_structure pos;
      if validate then validate_and_map pos else E.return pos
    | sections -> E.fail @@ Invalid_number_of_sections (List.length sections)

  let of_string_exn ?(validate = true) s =
    of_string s ~validate |> Result.map_error ~f:Error.to_string |> function
    | Error e -> invalid_argf "Failed to parse FEN string '%s': %s" s e ()
    | Ok pos -> pos
end

let pp ppf pos = Format.fprintf ppf "%s" @@ Fen.to_string pos

let start = Fen.(of_string_exn start)

(* Handling moves.

   This is likely to be the biggest source of bugs, as it is quite easy to
   incorrectly implement the rules of chess.

   NOTE: We originally used a state monad for the Makemove module, and a reader
   monad for the Movegen module. For performance reasons, we then switched to a
   reader monad for Makemove, with mutable fields in the position datatype.
   This pattern acted as a "pseudo" state monad. However, we've gotten rid of
   the monadic code entirely since the compiler had trouble with inlining and
   specialization, as there were a lot of calls to anonymous functions left in
   the compiled code. While the monadic style was much more concise and elegant,
   our priority for this code weighs far more in favor of performance.
*)

module Makemove = struct
  (* `en_passant_pawn` is the pawn that is "in front" of the en passant
     square. This field is only valid if the move we are making is in fact an
     en passant capture.

     `castle` is the side on which castling occurred, if any.

     `piece` is the piece being moved (which belongs to the active color).

     `direct_capture` is the inactive piece, if any, that will be captured
     by the "direct" move (e.g. it is not an en passant capture).
  *)
  type context = {
    en_passant_pawn : Square.t Uopt.t;
    castle_side : Cr.side Uopt.t;
    piece : Piece.t;
    direct_capture : Piece.t Uopt.t;
  } [@@deriving fields]

  let[@inline] update_hash pos ~f = set_hash pos @@ f pos.hash

  let[@inline] update_pawn_hash sq c pos =
    set_pawn_hash pos @@ Hash.Update.piece c Pawn sq pos.pawn_hash

  let[@inline] map_color c pos ~f = match c with
    | Piece.White -> set_white pos @@ f @@ white pos
    | Piece.Black -> set_black pos @@ f @@ black pos

  let[@inline] map_kind k pos ~f = match k with
    | Piece.Pawn   -> set_pawn pos   @@ f @@ pawn pos
    | Piece.Knight -> set_knight pos @@ f @@ knight pos
    | Piece.Bishop -> set_bishop pos @@ f @@ bishop pos
    | Piece.Rook   -> set_rook pos   @@ f @@ rook pos
    | Piece.Queen  -> set_queen pos  @@ f @@ queen pos
    | Piece.King   -> set_king pos   @@ f @@ king pos

  (* Helper for setting both the color and the kind fields of the board. *)
  let[@inline] map_piece p sq pos ~f = 
    let c, k = Piece.decomp p in
    map_color c pos ~f;
    map_kind k pos ~f;
    update_hash pos ~f:(Hash.Update.piece c k sq);
    begin match k with
      | Pawn -> update_pawn_hash sq c pos
      | _ -> ()
    end;
    k

  let set = Fn.flip Bb.set
  let clr = Fn.flip Bb.clear

  let[@inline] set_square p sq pos = map_piece p sq pos ~f:(set sq) |> ignore
  let[@inline] clear_square p sq pos = map_piece p sq pos ~f:(clr sq) |> ignore

  (* Assume that if `p` exists, then it occupies square `sq`. *)
  let[@inline] clear_square_capture sq direct_capture pos =
    if Uopt.is_some direct_capture then
      let p = Uopt.unsafe_value direct_capture in
      map_piece p sq pos ~f:(clr sq) |> Uopt.some
    else Uopt.none

  (* The halfmove clock is reset after captures and pawn moves, and incremented
     otherwise. *)
  let[@inline] update_halfmove ctx pos = set_halfmove pos @@
    if Uopt.is_some ctx.en_passant_pawn
    || Uopt.is_some ctx.direct_capture
    || Piece.is_pawn ctx.piece
    then 0 else succ pos.halfmove

  (* Castling rights change monotonically, so the only time we update the hash
     is when we take away rights. *)
  let[@inline] castle_hash c s pos =
    if Cr.mem pos.castle c s then update_hash pos ~f:(Hash.Update.castle c s)

  let[@inline] clear_white_castling_rights pos =
    castle_hash White Kingside pos;
    castle_hash White Queenside pos;
    set_castle pos @@ Cr.(minus pos.castle white)

  let[@inline] clear_black_castling_rights pos =
    castle_hash Black Kingside pos;
    castle_hash Black Queenside pos;
    set_castle pos @@ Cr.(minus pos.castle black)

  let[@inline] white_kingside_castle pos =
    clear_square Piece.white_rook Square.h1 pos;
    set_square Piece.white_rook Square.f1 pos;
    clear_white_castling_rights pos

  let[@inline] white_queenside_castle pos =
    clear_square Piece.white_rook Square.a1 pos;
    set_square Piece.white_rook Square.d1 pos;
    clear_white_castling_rights pos

  let[@inline] black_kingside_castle pos =
    clear_square Piece.black_rook Square.h8 pos;
    set_square Piece.black_rook Square.f8 pos;
    clear_black_castling_rights pos

  let[@inline] black_queenside_castle pos =
    clear_square Piece.black_rook Square.a8 pos;
    set_square Piece.black_rook Square.d8 pos;
    clear_black_castling_rights pos

  (* If we're castling our king on this move, then we need to move the rook as
     well as clear our rights. *)
  let[@inline] king_moved_or_castled castle pos =
    if Uopt.is_some castle then match pos.active, Uopt.unsafe_value castle with
      | Piece.White, Cr.Kingside  -> white_kingside_castle pos
      | Piece.White, Cr.Queenside -> white_queenside_castle pos
      | Piece.Black, Cr.Kingside  -> black_kingside_castle pos
      | Piece.Black, Cr.Queenside -> black_queenside_castle pos
    else match pos.active with
      | Piece.White -> clear_white_castling_rights pos
      | Piece.Black -> clear_black_castling_rights pos

  (* If we're moving or capturing a rook, then clear the castling rights for
     that particular side. *)
  let[@inline] rook_moved_or_captured sq c pos = match c with
    | Piece.White when Square.(sq = h1) ->
      castle_hash White Kingside pos;
      set_castle pos @@ Cr.(minus pos.castle white_kingside)
    | Piece.White when Square.(sq = a1) ->
      castle_hash White Queenside pos;
      set_castle pos @@ Cr.(minus pos.castle white_queenside)
    | Piece.Black when Square.(sq = h8) ->
      castle_hash Black Kingside pos;
      set_castle pos @@ Cr.(minus pos.castle black_kingside)
    | Piece.Black when Square.(sq = a8) ->
      castle_hash Black Queenside pos;
      set_castle pos @@ Cr.(minus pos.castle black_queenside)
    | _ -> ()

  (* Rook moved from a square. *)
  let[@inline] rook_moved src pos = rook_moved_or_captured src pos.active pos

  (* Rook was captured at a square. Assume that it is the inactive's color. *)
  let[@inline] rook_captured dst direct_capture pos =
    if Uopt.is_some direct_capture then
      let p = Uopt.unsafe_value direct_capture in
      if Piece.is_rook p then rook_moved_or_captured dst (Piece.color p) pos

  (* Handle castling-related details. *)
  let[@inline] update_castle ctx src dst pos =
    begin match Piece.kind ctx.piece with
      | Rook -> rook_moved src pos
      | King -> king_moved_or_castled ctx.castle_side pos
      | _ -> ()
    end;
    rook_captured dst ctx.direct_capture pos

  (* Reset the en passant hash and return the new en passant square if a pawn
     double push occurred. *) 
  let[@inline] setup_en_passant p src dst pos =
    update_hash pos ~f:(Hash.Update.en_passant pos.en_passant);
    if Piece.is_pawn p then
      let rank, file = Square.decomp src in
      let rank' = Square.rank dst in
      match pos.active with
      | Piece.White when rank' - rank = 2 ->
        Uopt.some @@ Square.(with_rank_unsafe dst Rank.three)
      | Piece.Black when rank - rank' = 2 ->
        Uopt.some @@ Square.(with_rank_unsafe dst Rank.six)
      | _ -> Uopt.none
    else Uopt.none

  (* After each halfmove, give the turn to the other player. *)
  let[@inline] flip_active pos =
    update_hash pos ~f:Hash.Update.active_player;
    set_active pos @@ inactive pos

  (* Since white moves first, increment the fullmove clock after black
     has moved. *)
  let[@inline] update_fullmove pos = match pos.active with
    | Black -> set_fullmove pos @@ succ pos.fullmove
    | White -> ()

  (* Update the piece for the destination square if we're promoting. *)
  let[@inline] do_promote p promote pos = match promote with
    | Some k -> Piece.with_kind p k
    | None -> p

  (* Clear the square of the pawn captured by en passant. *)
  let[@inline] en_passant_capture pw pos =
    if Uopt.is_some pw then
      let sq = Uopt.unsafe_value pw in
      let p = Piece.create (inactive pos) Pawn in
      clear_square p sq pos;
      Uopt.some Piece.Pawn
    else Uopt.none

  (* Set the en passant square for the resulting position, but only if the
     square is threatened by an opposing pawn. *)
  let[@inline] update_en_passant ep capture pos =
    let ep =
      if Uopt.is_some ep && Uopt.is_none capture then
        let sq = Uopt.unsafe_value ep in
        if has_en_passant_threat_aux pos sq then
          let file = Square.file sq in
          update_hash pos ~f:(Hash.Update.en_passant_file file);
          ep
        else Uopt.none
      else Uopt.none in
    set_en_passant pos ep

  let[@inline] go src dst promote ctx pos =
    (* Do the stuff that relies on the initial state. *)
    let ep = setup_en_passant ctx.piece src dst pos in
    update_halfmove ctx pos;
    update_castle ctx src dst pos;
    (* Clear the old placement. *)
    clear_square ctx.piece src pos;
    let capture = clear_square_capture dst ctx.direct_capture pos in
    (* Set the new placement. *)
    let p = do_promote ctx.piece promote pos in
    set_square p dst pos;
    let capture =
      if Uopt.is_some capture then capture
      else en_passant_capture ctx.en_passant_pawn pos in
    (* Prepare for the next move. *)
    update_fullmove pos;
    flip_active pos;
    update_en_passant ep capture pos;
    (* Return the capture that was made, if any. *)
    capture 
end

module Legal = struct
  module T = struct
    type t = {
      move : Move.t;
      parent : T.t;
      new_position : T.t;
      capture : Piece.kind Uopt.t;
      is_en_passant : bool;
      castle_side : Cr.side Uopt.t;
    } [@@deriving compare, equal, sexp, fields]

    let same l1 l2 =
      same_hash l1.parent l2.parent &&
      same_hash l1.new_position l2.new_position

    let is_move legal m = Move.(m = legal.move)
    let capture legal = Uopt.to_option legal.capture
    let castle_side legal = Uopt.to_option legal.castle_side

    let capture_square legal =
      if Uopt.is_none legal.capture then None
      else Option.some @@
        let dst = Move.dst legal.move in
        if not legal.is_en_passant then dst
        else Fn.flip en_passant_pawn_aux dst @@ inactive legal.new_position
  end

  let sort moves ~eval =
    List.map moves ~f:(fun m -> m, eval m) |>
    List.sort ~compare:(fun (_, a) (_, b) -> Int.compare b a) |>
    List.map ~f:fst

  let best moves ~eval =
    let open Option.Monad_infix in
    List.filter_map moves ~f:(fun m -> eval m >>| fun score -> (m, score)) |>
    List.sort ~compare:(fun (_, a) (_, b) -> Int.compare b a) |>
    List.fold_until ~init:([], 0) ~finish:fst
      ~f:(fun (acc, score') (m, score) -> match acc with
          | [] -> Continue (m :: acc, score)
          | _ when score' > score -> Stop acc
          | _ -> Continue (m :: acc, score'))

  include T
  include Comparable.Make(T)
end

type legal = Legal.t [@@deriving compare, equal, sexp]

module Movegen = struct
  open Analysis

  module Pieces = struct
    module Pawn = struct
      let[@inline] push sq {pos; occupied; _} =
        Bb.(Pre.pawn_advance sq pos.active - occupied)

      (* Double pawn push. *)
      let[@inline] push2 rank file {pos; occupied; _} =
        let open Bb.Syntax in
        match pos.active with
        | Piece.White when Square.Rank.(rank = two) ->
          !!(Square.create_unsafe ~rank:Square.Rank.four ~file) - occupied
        | Piece.Black when Square.Rank.(rank = seven) ->
          !!(Square.create_unsafe ~rank:Square.Rank.five ~file) - occupied
        | _ -> Bb.empty

      (* Check if our pawn or the captured pawn is along a pin ray. If so,
         then this capture would be illegal, since it would lead to a discovery
         on the king. En passant moves arise rarely across all chess positions,
         so we can do a bit of heavy calculation here. *)
      let[@inline] en_passant sq ep pw diag
          {king_sq; occupied; inactive_sliders; _} = let open Bb.Syntax in
        (* Remove our pawn and the captured pawn from the board, but pretend
           that the en passant square is occupied. This covers the case where
           we can capture the pawn, but it may leave our pawn pinned. *)
        let occupied = occupied -- sq -- pw ++ ep in
        let init = diag ++ ep and finish = ident in
        (* Check if an appropriate diagonal attack from the king would reach
           that corresponding piece. *)
        let bishop = Pre.bishop king_sq occupied in
        let rook   = Pre.rook   king_sq occupied in
        let queen  = Pre.queen  king_sq occupied in
        (List.fold_until [@specialised]) inactive_sliders ~init ~finish
          ~f:(fun acc -> function
              | sq, Piece.Bishop when sq @ bishop -> Stop diag
              | sq, Piece.Rook   when sq @ rook   -> Stop diag
              | sq, Piece.Queen  when sq @ queen  -> Stop diag
              | _ -> Continue acc)

      let[@inline] capture sq
          ({pos; en_passant_pawn = pw; inactive_board; _} as a) =
        let open Bb.Syntax in
        let capture = Pre.pawn_capture sq pos.active in
        let diag = capture & inactive_board in
        if Uopt.is_some pw then
          let ep, pw = Uopt.(unsafe_value pos.en_passant, unsafe_value pw) in
          if ep @ capture then en_passant sq ep pw diag a
          else diag
        else diag

      let promote_kinds = Piece.[Knight; Bishop; Rook; Queen]

      (* We need to multiply the move by the number of pieces we can
         promote to. *)
      let[@inline] promote src dst =
        List.map promote_kinds ~f:(Move.create_with_promote src dst)
    end

    module Knight = struct
      let[@inline] jump sq {active_board; _} = Bb.(Pre.knight sq - active_board)
    end

    module Bishop = struct
      let[@inline] slide sq {occupied; active_board; _} =
        Bb.(Pre.bishop sq occupied - active_board)
    end

    module Rook = struct
      let[@inline] slide sq {occupied; active_board; _} =
        Bb.(Pre.rook sq occupied - active_board)
    end

    module Queen = struct
      let[@inline] slide sq {occupied; active_board; _} =
        Bb.(Pre.queen sq occupied - active_board)
    end

    module King = struct
      (* Note that `inactive_attacks` includes squares occupied by inactive
         pieces. Therefore, the king may not attack those squares. *)
      let[@inline] move sq {active_board; inactive_attacks; _} =
        Bb.(Pre.king sq - (active_board + inactive_attacks))

      let castle {pos; occupied; inactive_attacks; num_checkers; _} =
        let open Bb in
        (* Cannot castle out of check. *)
        if Int.(num_checkers = 0) then
          let active = pos.active in
          let m = occupied + inactive_attacks in
          let[@inline] ks_castle sq =
            let b = Pre.castle pos.castle active Kingside in
            if Int.equal 2 @@ count (b - m) then !!sq else empty in
          let[@inline] qs_castle sq bsq =
            if not (bsq @ occupied) then
              let b = Pre.castle pos.castle active Queenside in
              if Int.equal 2 @@ count (b - m) then !!sq else empty
            else empty in
          let ks, qs, bsq = match active with
            | Piece.White -> Square.(g1, c1, b1)
            | Piece.Black -> Square.(g8, c8, b8) in
          ks_castle ks + qs_castle qs bsq
        else empty
    end

    (* Use this mask to restrict the movement of pinned pieces. *)
    let[@inline] pin_mask sq {king_sq; pinners; _} = let open Bb in
      let p = Array.unsafe_get pinners @@ Square.to_int sq in
      match count p with
      | 1 ->
        let pinner = first_set_exn p in
        Pre.between king_sq pinner ++ pinner
      | 0 -> full
      | _ -> empty

    (* Special case for pawns, with en passant capture being an option to
       escape check. *)
    let[@inline] check_mask_pawn capture
        {num_checkers; check_mask; en_passant_check_mask; _} =
      if num_checkers <> 1 then check_mask
      else Bb.(check_mask + (capture & en_passant_check_mask))

    (* Pawn has special case for check mask. *)
    let[@inline] make_pawn sq b capture a =
      Bb.(b & pin_mask sq a & check_mask_pawn capture a)

    (* All other pieces (except the king). *)
    let[@inline] make sq a b = Bb.(b & pin_mask sq a & a.check_mask)

    let[@inline] pawn sq a =
      let open Pawn in
      let open Bb.Syntax in
      let push = push sq a in
      (* Only allow double push if a single push is available. *)
      let push = if Bb.(push <> empty) then
          let rank, file = Square.decomp sq in
          push + push2 rank file a
        else push in
      let capture = capture sq a in
      make_pawn sq (push + capture) capture a

    let[@inline] knight sq a = make sq a @@ Knight.jump  sq a
    let[@inline] bishop sq a = make sq a @@ Bishop.slide sq a
    let[@inline] rook   sq a = make sq a @@ Rook.slide   sq a
    let[@inline] queen  sq a = make sq a @@ Queen.slide  sq a

    let[@inline] king sq a =
      let open King in
      let open Bb.Syntax in
      move sq a + castle a
  end

  let[@inline] castle_side piece src dst =
    if Piece.is_king piece then
      let file = Square.file src in
      if Square.File.(file = e) then
        let file' = Square.file dst in
        if Square.File.(file' = c) then Uopt.some Cr.Queenside
        else if Square.File.(file' = g) then Uopt.some Cr.Kingside
        else Uopt.none
      else Uopt.none
    else Uopt.none

  (* Actually runs the makemove routine and returns relevant info. *)
  let[@inline] run_makemove pos ~src ~dst ~promote ~piece ~en_passant_pawn =
    let new_position = copy pos in
    let direct_capture = piece_at_square_uopt pos dst in
    let is_en_passant = Piece.is_pawn piece && is_en_passant pos dst in
    let en_passant_pawn =
      if is_en_passant then en_passant_pawn else Uopt.none in
    let castle_side = castle_side piece src dst in
    let ctx = Makemove.Fields_of_context.create
        ~en_passant_pawn ~piece ~castle_side ~direct_capture in
    let capture = Makemove.go src dst promote ctx new_position in
    new_position, capture, is_en_passant, castle_side

  (* Accumulator function for a list of moves. *)
  let[@inline] accum_makemove acc move ~pos:parent ~en_passant_pawn ~piece =
    let src, dst, promote = Move.decomp move in
    let new_position, capture, is_en_passant, castle_side =
      run_makemove parent ~src ~dst ~promote ~piece ~en_passant_pawn in
    Legal.Fields.create
      ~move ~parent ~new_position ~capture ~is_en_passant ~castle_side :: acc

  (* If we're promoting, then the back rank should be the only
     available squares. *)
  let[@inline] is_promote_rank b = function
    | Piece.White -> Bb.((b & rank_8) = b)
    | Piece.Black -> Bb.((b & rank_1) = b)

  (* Get the list of moves from the bitboard of squares we can move to. *)
  let[@inline] bb_to_moves src k b ~init ~a:{pos; en_passant_pawn; _} =
    let active = pos.active in
    let piece = Piece.create active k in
    let f = accum_makemove ~pos ~en_passant_pawn ~piece in
    match k with
    | Piece.Pawn when is_promote_rank b active -> 
      (Bb.fold [@specialised]) b ~init ~f:(fun init dst ->
          Pieces.Pawn.promote src dst |> List.fold ~init ~f)
    | _ ->
      (Bb.fold [@specialised]) b ~init
        ~f:(fun acc dst -> Move.create src dst |> f acc)

  (* Piece-specific bitboards for legal moves. *)
  let[@inline] bb_of_kind sq k a = match k with
    | Piece.Pawn   -> Pieces.pawn   sq a
    | Piece.Knight -> Pieces.knight sq a
    | Piece.Bishop -> Pieces.bishop sq a
    | Piece.Rook   -> Pieces.rook   sq a
    | Piece.Queen  -> Pieces.queen  sq a
    | Piece.King   -> Pieces.king   sq a

  let[@inline] go ({pos; king_sq; num_checkers; _} as a) =
    (* If the king has more than one attacker, then it is the only piece
       we can move. *)
    if num_checkers > 1
    then Pieces.king king_sq a |> bb_to_moves king_sq King ~init:[] ~a
    else (List.fold [@specialised]) ~init:[] ~f:(fun init (sq, k) ->
        bb_of_kind sq k a |> bb_to_moves sq k ~init ~a) @@ collect_active pos
end

let legal_moves pos = Movegen.go @@ Analysis.create pos

let make_move pos move =
  match legal_moves pos |> List.find ~f:(Fn.flip Legal.is_move move) with
  | None -> invalid_argf "Move %s is not legal" (Move.to_string move) ()
  | Some legal -> legal

let null_move_unsafe pos =
  let pos = copy pos in
  Makemove.flip_active pos;
  Makemove.update_hash pos ~f:(Hash.Update.en_passant pos.en_passant);
  set_en_passant pos Uopt.none;
  set_halfmove pos 0;
  pos

let null_move pos =
  if in_check pos
  then invalid_argf "Illegal null move on position %s" (Fen.to_string pos) ()
  else null_move_unsafe pos

(* Standard Algebraic Notation (SAN). *)

module San = struct
  let disambiguate parent k ~addc ~adds ~src ~dst =
    let a = Analysis.create parent in
    (* More than one checker means it's a king move, which is unambiguous. *)
    if a.Analysis.num_checkers <= 1 then
      let rank, file = Square.decomp src in
      (* Find all the other pieces of the same kind and generate their move
         bitboards. *)
      collect_kind parent k |> List.filter ~f:(fun (sq, c) ->
          Square.(sq <> src) && Piece.Color.(c = parent.active)) |>
      List.map ~f:(fun (sq, _) -> sq, Movegen.bb_of_kind sq k a) |>
      List.filter ~f:(fun (_, b) -> Bb.(dst @ b)) |> function
      | [] -> ()
      | moves ->
        let search x f =
          not @@ List.exists moves ~f:(fun (sq, _) -> f sq = x) in
        (* First try to distinguish by file, then by rank, and finally the
           departing square. *)
        if search file Square.file then addc @@ Square.File.to_char file
        else if search rank Square.rank then addc @@ Square.Rank.to_char rank
        else adds @@ Square.to_string src

  let of_legal legal =
    let buf = Buffer.create 16 in
    let adds = Buffer.add_string buf in
    let addc = Buffer.add_char buf in
    let src, dst, promote = Move.decomp @@ Legal.move legal in
    let pos = Legal.new_position legal in
    let num_checkers =
      let king_sq =
        List.hd_exn @@ collect_piece pos @@ Piece.create pos.active King in
      let occupied = all_board pos in
      let inactive_board = inactive_board pos in
      Bb.count @@ Analysis.checkers pos ~king_sq ~inactive_board ~occupied in
    let checkmate = num_checkers <> 0 && List.is_empty @@ legal_moves pos in
    begin match Legal.castle_side legal with
      (* Castling *)
      | Some Cr.Kingside -> adds "O-O"
      | Some Cr.Queenside -> adds "O-O-O"
      | None ->
        let p = piece_at_square_exn pos dst in
        let p = match promote with
          | Some _ -> Piece.with_kind p Pawn
          | None -> p in
        (* Piece being moved *)
        let dis = disambiguate ~addc ~adds ~src ~dst @@ Legal.parent legal in
        begin match Piece.kind p with
          | Piece.Pawn -> if Uopt.is_none legal.capture
            then adds @@ Square.to_string dst
            else addc @@ Square.file_char src
          | Piece.Knight -> addc 'N'; dis Knight
          | Piece.Bishop -> addc 'B'; dis Bishop
          | Piece.Rook   -> addc 'R'; dis Rook
          | Piece.Queen  -> addc 'Q'; dis Queen
          | Piece.King   -> addc 'K'
        end;
        (* Capture *)
        Uopt.to_option legal.capture |> Option.iter ~f:(fun _ -> addc 'x');
        (* Destination *)
        if not (Piece.is_pawn p && Uopt.is_none legal.capture)
        then adds @@ Square.to_string dst;
        (* Promotion *)
        Option.iter promote ~f:(function
            | Piece.Knight -> adds "=N"
            | Piece.Bishop -> adds "=B"
            | Piece.Rook   -> adds "=R"
            | Piece.Queen  -> adds "=Q"
            | _ -> assert false);
    end;
    (* Checkmate or check *)
    if checkmate then addc '#'
    else if num_checkers = 1 then addc '+'
    else if num_checkers = 2 then adds "++";
    Buffer.contents buf
end

include Comparable.Make(T)
