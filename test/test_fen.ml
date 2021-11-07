open Core_kernel
open OUnit2
open Chess

let test_starting_position () =
  let s = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" in
  let fen = Fen.of_string_exn s in
  assert_equal fen.placement
    (Map.of_alist_exn
       (module Square)
       [ (Square.a1, Piece.white_rook)
       ; (Square.b1, Piece.white_knight)
       ; (Square.c1, Piece.white_bishop)
       ; (Square.d1, Piece.white_queen)
       ; (Square.e1, Piece.white_king)
       ; (Square.f1, Piece.white_bishop)
       ; (Square.g1, Piece.white_knight)
       ; (Square.h1, Piece.white_rook)
       ; (Square.a2, Piece.white_pawn)
       ; (Square.b2, Piece.white_pawn)
       ; (Square.c2, Piece.white_pawn)
       ; (Square.d2, Piece.white_pawn)
       ; (Square.e2, Piece.white_pawn)
       ; (Square.f2, Piece.white_pawn)
       ; (Square.g2, Piece.white_pawn)
       ; (Square.h2, Piece.white_pawn)
       ; (Square.a7, Piece.black_pawn)
       ; (Square.b7, Piece.black_pawn)
       ; (Square.c7, Piece.black_pawn)
       ; (Square.d7, Piece.black_pawn)
       ; (Square.e7, Piece.black_pawn)
       ; (Square.f7, Piece.black_pawn)
       ; (Square.g7, Piece.black_pawn)
       ; (Square.h7, Piece.black_pawn)
       ; (Square.a8, Piece.black_rook)
       ; (Square.b8, Piece.black_knight)
       ; (Square.c8, Piece.black_bishop)
       ; (Square.d8, Piece.black_queen)
       ; (Square.e8, Piece.black_king)
       ; (Square.f8, Piece.black_bishop)
       ; (Square.g8, Piece.black_knight)
       ; (Square.h8, Piece.black_rook) ] )
    ~cmp:(Base.Map.equal Piece.equal);
  assert_equal fen.active Piece.White ~cmp:Piece.Color.equal;
  assert_equal fen.queenside_castle
    (Set.of_list (module Piece.Color) [White; Black])
    ~cmp:Base.Set.equal;
  assert_equal fen.kingside_castle
    (Set.of_list (module Piece.Color) [White; Black])
    ~cmp:Base.Set.equal;
  assert_equal fen.en_passant None ~cmp:(Option.equal Square.equal);
  assert_equal fen.halfmove 0 ~cmp:Int.equal;
  assert_equal fen.fullmove 1 ~cmp:Int.equal;
  assert_equal (Fen.to_string fen) s ~cmp:String.equal

let suite =
  "Test FEN"
  >::: [("Starting position" >:: fun _ -> test_starting_position ())]

let () = run_test_tt_main suite
