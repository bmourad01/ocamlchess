#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/version.h>

#include <SFML/Graphics.h>

/* Calling to OCaml */

static const value *_piece_at_square, *_string_of_square;

/*
  Assets

  XXX: don't rely on system installation for these

 */

static const char *const _piece_font_filename =
    "/usr/share/fonts/TTF/FreeSerif.ttf";
static const char *const _text_font_filename =
    "/usr/share/fonts/TTF/FreeSans.ttf";

static sfFont *_piece_font, *_text_font;

/* Helpers */

static const value *named_value_or_fail(const char *name) {
  const value *v = caml_named_value(name);
  if (v) {
    return v;
  } else {
    fprintf(stderr, "Named value %s was not found\n", name);
    abort();
  }
}

static void init_font_or_fail(sfFont **font, const char *name) {
  assert(font);
  if (!(*font = sfFont_createFromFile(name))) {
    fprintf(stderr, "Couldn't load font %s\n", name);
    abort();
  }
}

enum Color { White, Black };
enum Kind { Pawn, Knight, Bishop, Rook, Queen, King };

static wchar_t piece_unicode(int color, int kind) {
  bool is_white = color == White;
  switch (kind) {
  case Pawn:
    return is_white ? L'\u2659' : L'\u265F';
  case Knight:
    return is_white ? L'\u2658' : L'\u265E';
  case Bishop:
    return is_white ? L'\u2657' : L'\u265D';
  case Rook:
    return is_white ? L'\u2656' : L'\u265C';
  case Queen:
    return is_white ? L'\u2655' : L'\u265B';
  case King:
    return is_white ? L'\u2654' : L'\u265A';
  default:
    assert(false);
  }
}

#define Sfml_window_val(v) (*(sfRenderWindow**)(Data_custom_val(v)))
#define Make_square(x, y) ((unsigned int)(((y) << 3) | (x)))
#define Move_src(m) (Int_val(m) & 0b111111)
#define Move_dst(m) ((Int_val(m) >> 6) & 0b111111)
#define Piece_color(p) ((Int_val(p) >> 3) & 0b1)
#define Piece_kind(p) (Int_val(p) & 0b111)

/* Custom allocation for the SFML window object */

static void sfml_finalize_window(value window) {
  CAMLparam1(window);
  sfRenderWindow_destroy(Sfml_window_val(window));
  CAMLreturn0;
}

static struct custom_operations sfml_window_custom_ops = {
  (char *)"sfml_window_custom_ops",
  sfml_finalize_window,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
#if OCAML_VERSION_MAJOR >= 4 && OCAML_VERSION_MINOR >= 8
  NULL, // custom_fixed_length
#endif
};

/* Exposed minimal API */

value ml_init_fonts(value dummy) {
  CAMLparam1(dummy);
  init_font_or_fail(&_piece_font, _piece_font_filename);
  init_font_or_fail(&_text_font, _text_font_filename);
  CAMLreturn(Val_unit);
}

value ml_init_named_values(value dummy) {
  CAMLparam1(dummy);
  _piece_at_square = named_value_or_fail("piece_at_square");
  _string_of_square = named_value_or_fail("string_of_square");
  CAMLreturn(Val_unit);
}

value ml_window_create(value w, value h, value name) {
  CAMLparam3(w, h, name);
  CAMLlocal1(window);

  sfVideoMode video_mode = sfVideoMode_getDesktopMode();
  video_mode.width = Int_val(w);
  video_mode.height = Int_val(h);
  
  sfRenderWindow *sf_window = sfRenderWindow_create(video_mode,
                                                    String_val(name),
                                                    sfTitlebar|sfClose,
                                                    NULL);

  window = caml_alloc_custom(&sfml_window_custom_ops,
                             sizeof(sfRenderWindow *), 0, 1);

  Sfml_window_val(window) = sf_window;
  CAMLreturn(window);
}

value ml_window_size(value window) {
  CAMLparam1(window);
  CAMLlocal1(result);

  sfRenderWindow *sf_window = Sfml_window_val(window);
  sfVector2u size = sfRenderWindow_getSize(sf_window);
  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(size.x));
  Store_field(result, 1, Val_int(size.y));
  CAMLreturn(result);
}

value ml_window_is_open(value window) {
  CAMLparam1(window);
  sfRenderWindow *sf_window = Sfml_window_val(window);
  CAMLreturn(Val_bool(sfRenderWindow_isOpen(sf_window)));
}

value ml_window_poll_event(value window) {
  CAMLparam1(window);
  CAMLlocal2(result, mouse_coords);

  sfEvent event;
  sfRenderWindow *sf_window = Sfml_window_val(window);
  result = Val_none;
  while (sfRenderWindow_pollEvent(sf_window, &event)) {
    switch (event.type) {
    case sfEvtClosed:
      sfRenderWindow_close(sf_window);
      break;
    case sfEvtMouseButtonPressed:
      result = caml_alloc(2, 0);
      Store_field(result, 0, Val_int(hash_variant("Mouse_button_pressed")));
      mouse_coords = caml_alloc_tuple(2);
      Store_field(mouse_coords, 0, Val_int(event.mouseButton.x));
      Store_field(mouse_coords, 1, Val_int(event.mouseButton.y));
      Store_field(result, 1, mouse_coords);
      result = caml_alloc_some(result);
      break;
    default:
      break;
    }
  }

  CAMLreturn(result);
}

value ml_window_close(value window) {
  CAMLparam1(window);
  sfRenderWindow *sf_window = Sfml_window_val(window);
  sfRenderWindow_close(sf_window);
  CAMLreturn(Val_unit);
}

value ml_window_clear(value window) {
  CAMLparam1(window);
  sfRenderWindow *sf_window = Sfml_window_val(window);
  sfRenderWindow_clear(sf_window, sfBlack);
  CAMLreturn(Val_unit);
}

value ml_window_paint_board(value window, value pos, value valid_squares,
                            value selected_square, value prev_move) {
  CAMLparam5(window, pos, valid_squares, selected_square, prev_move);
  CAMLlocal2(piece, sq_string);

  sfRenderWindow *sf_window = Sfml_window_val(window);
  sfVector2u size = sfRenderWindow_getSize(sf_window);
  uint64_t bb_valid = Int64_val(valid_squares);
  int tw = size.x / 8;
  int th = size.y / 8;
  int sz = (tw + th) / 2;
  int sz2 = (tw + th) / 12;

  for (int y = 0; y < 8; ++y) {
    for (int x = 0; x < 8; ++x) {
      // Current square we're iterating over.
      unsigned int sq = Make_square(x, 7 - y);

      // Draw the board's squares first, with some nice colors
      // depending on the state.
      sfRectangleShape *tile = sfRectangleShape_create();
      sfVector2f tile_size;
      sfColor color;
      tile_size.x = tw;
      tile_size.y = th;
      sfRectangleShape_setSize(tile, tile_size);
      if (selected_square != Val_none &&
          Int_val(Some_val(selected_square)) == sq) {
        // This is the square we selected to make a move.
        color.a = 0xFF;
        color.r = 0xF3;
        color.g = 0xF7;
        color.b = 0x81;
      } else if (bb_valid & (1ULL << sq)) {
        // This square represents a valid destination for a move.
        color.a = 0xFF;
        color.r = 0x2E;
        color.g = 0xCC;
        color.b = 0xFA;
      } else if (prev_move != Val_none &&
                 (Move_src(Some_val(prev_move)) == sq ||
                  Move_dst(Some_val(prev_move)) == sq)) {
        // These squares represent the last move that was made.
        color.a = 0xFF;
        color.r = 0xCC;
        color.g = 0xCC;
        color.b = 0xCC;
      } else {
        // Use the normal alternating colors for the square.
        if (y % 2) {
          if (x % 2) {
            color.a = 0xFF;
            color.r = 0xFF;
            color.g = 0xFF;
            color.b = 0xFF;
          } else {
            color.a = 0xFF;
            color.r = 0xF7;
            color.g = 0xF2;
            color.b = 0xF0;
          }
        } else {
          if (x % 2) {
            color.a = 0xFF;
            color.r = 0xF7;
            color.g = 0xF2;
            color.b = 0xF0;
          } else {
            color.a = 0xFF;
            color.r = 0xFF;
            color.g = 0xFF;
            color.b = 0xFF;
          }
        }
      }
      sfVector2f position;
      position.x = x * tw;
      position.y = y * th;
      sfRectangleShape_setFillColor(tile, color);
      sfRectangleShape_setPosition(tile, position);
      sfRenderWindow_drawRectangleShape(sf_window, tile, NULL);
      sfRectangleShape_destroy(tile);

      // Draw the piece at the current square (if any).
      piece = caml_callback2(*_piece_at_square, pos, Val_int(sq));
      if (piece != Val_none) {
        wchar_t u = piece_unicode(Piece_color(Some_val(piece)),
                                  Piece_kind(Some_val(piece)));
        sfUint32 unicode[2] = {(sfUint32)u, 0};
        float fx = x * tw;
        float fy = y * th;
        float xa = ((float)(tw - sz) / 2.0f) + ((float)sz / 8.5f);
        float ya = ((float)(th - sz) / 10.0f) - ((float)sz / 5.0f);
        sfVector2f position;
        position.x = fx + xa;
        position.y = fy + ya;
        
        sfText *text = sfText_create();
        sfText_setUnicodeString(text, unicode);
        sfText_setFont(text, _piece_font);
        sfText_setCharacterSize(text, sz);
        sfText_setPosition(text, position);
        sfText_setColor(text, sfBlack);
        sfRenderWindow_drawText(sf_window, text, NULL);
        sfText_destroy(text);
      }

      // Draw the name of the square in algebraic notation.
      sq_string = caml_callback(*_string_of_square, Val_int(sq));
      const char *str = String_val(sq_string);
      sfText *text = sfText_create();
      color = sfBlack;
      color.a = 0x5F;
      sfText_setFont(text, _text_font);
      sfText_setPosition(text, position);
      sfText_setColor(text, color);
      sfText_setString(text, str);
      sfText_setCharacterSize(text, sz2);
      sfRenderWindow_drawText(sf_window, text, NULL);
      sfText_destroy(text);
    }
  }

  sfRenderWindow_display(sf_window);
  CAMLreturn(Val_unit);
}
