open Core_kernel [@@warning "-D"]
open OUnit2
open Chess

let cmp_bitboard = Bitboard.equal
let cmp_active = Piece.Color.equal
let cmp_castle = Castling_rights.equal
let cmp_en_passant = Option.equal Square.equal

let test_starting_position () =
  let pos = Position.start in
  assert_equal (Position.white pos) Bitboard.(rank_1 + rank_2)
    ~cmp:cmp_bitboard;
  assert_equal (Position.black pos) Bitboard.(rank_7 + rank_8)
    ~cmp:cmp_bitboard;
  assert_equal (Position.pawn pos) Bitboard.(rank_2 + rank_7)
    ~cmp:cmp_bitboard;
  assert_equal (Position.knight pos)
    Bitboard.(!!Square.b1 + !!Square.g1 + !!Square.b8 + !!Square.g8)
    ~cmp:cmp_bitboard;
  assert_equal (Position.bishop pos)
    Bitboard.(!!Square.c1 + !!Square.f1 + !!Square.c8 + !!Square.f8)
    ~cmp:cmp_bitboard;
  assert_equal (Position.rook pos)
    Bitboard.(!!Square.a1 + !!Square.h1 + !!Square.a8 + !!Square.h8)
    ~cmp:cmp_bitboard;
  assert_equal (Position.queen pos) Bitboard.(!!Square.d1 + !!Square.d8)
    ~cmp:cmp_bitboard;
  assert_equal (Position.king pos) Bitboard.(!!Square.e1 + !!Square.e8)
    ~cmp:cmp_bitboard;
  assert_equal (Position.active pos) Piece.White ~cmp:Piece.Color.equal;
  assert_equal (Position.castle pos) Castling_rights.all ~cmp:cmp_castle;
  assert_equal (Position.en_passant pos) None ~cmp:cmp_en_passant;
  assert_equal (Position.halfmove pos) 0;
  assert_equal (Position.fullmove pos) 1;
  assert_equal (Position.Fen.to_string pos) Position.Fen.start
    ~cmp:String.equal

let suite = "Test FEN" >::: [
    ("Starting position" >:: fun _ -> test_starting_position ());
  ]

let () = run_test_tt_main suite
