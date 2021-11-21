open Core_kernel
open OUnit2
open Chess

let cmp_bitboard = Bitboard.equal
let cmp_active = Piece.Color.equal
let cmp_castle = Castling_rights.equal
let cmp_en_passant = Option.equal Square.equal

let test_starting_position () =
  let pos = Fen.start in
  assert_equal pos.white Bitboard.(rank_1 + rank_2) ~cmp:cmp_bitboard;
  assert_equal pos.black Bitboard.(rank_7 + rank_8) ~cmp:cmp_bitboard;
  assert_equal pos.pawn Bitboard.(rank_2 + rank_7) ~cmp:cmp_bitboard;
  assert_equal pos.knight
    Bitboard.(!!Square.b1 + !!Square.g1 + !!Square.b8 + !!Square.g8)
    ~cmp:cmp_bitboard;
  assert_equal pos.bishop
    Bitboard.(!!Square.c1 + !!Square.f1 + !!Square.c8 + !!Square.f8)
    ~cmp:cmp_bitboard;
  assert_equal pos.rook
    Bitboard.(!!Square.a1 + !!Square.h1 + !!Square.a8 + !!Square.h8)
    ~cmp:cmp_bitboard;
  assert_equal pos.queen
    Bitboard.(!!Square.d1 + !!Square.d8)
    ~cmp:cmp_bitboard;
  assert_equal pos.king
    Bitboard.(!!Square.e1 + !!Square.e8)
    ~cmp:cmp_bitboard;
  assert_equal pos.active Piece.White ~cmp:Piece.Color.equal;
  assert_equal pos.castle Castling_rights.all ~cmp:cmp_castle;
  assert_equal pos.en_passant None ~cmp:cmp_en_passant;
  assert_equal pos.halfmove 0;
  assert_equal pos.fullmove 1;
  assert_equal (Fen.to_string pos) Fen.start_string ~cmp:String.equal

let suite =
  "Test FEN"
  >::: [("Starting position" >:: fun _ -> test_starting_position ())]

let () = run_test_tt_main suite
