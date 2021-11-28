#include <cassert>
#include <cstdio>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/version.h>

#include <SFML/Graphics.hpp>

static const char *const _piece_font_filename =
    "/usr/share/fonts/TTF/FreeSerif.ttf";
static const char *const _text_font_filename =
    "/usr/share/fonts/TTF/FreeSans.ttf";

static sf::Font _piece_font;
static sf::Font _text_font;

#define Sfml_window_val(v)                                                     \
  (*reinterpret_cast<sf::RenderWindow **>(Data_custom_val(window)))

#define Make_square(x, y) ((unsigned)(((y) << 3) | (x)))
#define Move_src(m) (Int_val(m) & 0b111111)
#define Move_dst(m) ((Int_val(m) >> 6) & 0b111111)
#define Piece_color(p) ((Int_val(p) >> 3) & 0b1)
#define Piece_kind(p) (Int_val(p) & 0b111)

extern "C" {
  value ml_init_fonts(value dummy) {
    CAMLparam1(dummy);
    CAMLlocal1(result);
    result = Val_bool(true);
    if (!_piece_font.loadFromFile(_piece_font_filename)) {
      fprintf(stderr, "couldn't load font %s\n", _piece_font_filename);
      result = Val_bool(false);
    }
    if (!_text_font.loadFromFile(_text_font_filename)) {
      fprintf(stderr, "couldn't load font %s\n", _text_font_filename);
      result = Val_bool(false);
    }
    CAMLreturn(result);
  }

  static void sfml_finalize_window(value window) {
    CAMLparam1(window);
    auto sf_window = Sfml_window_val(window);
    delete sf_window;
    CAMLreturn0;
  }
  
  static struct custom_operations sfml_window_custom_ops = {
    (char*)"sfml_window_custom_ops",
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

  value ml_window_create(value w, value h, value name) {
    CAMLparam3(w, h, name);
    CAMLlocal1(window);
    int width = Int_val(w);
    int height = Int_val(h);
    const char *name_ptr = String_val(name);
    auto sf_window = new sf::RenderWindow(sf::VideoMode(width, height), name_ptr);
    window = caml_alloc_custom(&sfml_window_custom_ops,
                               sizeof(sf::RenderWindow *), 0, 1);
    Sfml_window_val(window) = sf_window;
    CAMLreturn(window);
  }

  value ml_window_size(value window) {
    CAMLparam1(window);
    CAMLlocal1(tmp);
    auto size = Sfml_window_val(window)->getSize();
    tmp = caml_alloc(2, 0);
    Store_field(tmp, 0, Val_int(size.x));
    Store_field(tmp, 1, Val_int(size.y));
    CAMLreturn(tmp);
  }

  value ml_window_is_open(value window) {
    CAMLparam1(window);
    CAMLreturn(Val_bool(Sfml_window_val(window)->isOpen()));
  }

  value ml_window_poll_event(value window) {
    CAMLparam1(window);
    CAMLlocal2(result, mouse_coords);
    auto sf_window = Sfml_window_val(window);
    sf::Event event;
    result = Val_none;
    while (sf_window->pollEvent(event)) {
      switch (event.type) {
      case sf::Event::Closed:
        sf_window->close();
        break;
      case sf::Event::MouseButtonPressed:
        result = caml_alloc(2, 0);
        Store_field(result, 0, Val_int(hash_variant("MouseButtonPressed")));
        mouse_coords = caml_alloc(2, 0);
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

  value ml_window_clear(value window) {
    CAMLparam1(window);
    Sfml_window_val(window)->clear();
    CAMLreturn(Val_unit);
  }

  value ml_window_display(value window) {
    CAMLparam1(window);
    Sfml_window_val(window)->display();
    CAMLreturn(Val_unit);
  }

  enum Color : int {White, Black};
  enum Kind : int {Pawn, Knight, Bishop, Rook, Queen, King};

  static wchar_t piece_unicode(int color, int kind) {
    auto is_white = color == White;
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

  value ml_window_paint_board(value window,
                              value position,
                              value bitboard,
                              value square,
                              value prev) {
    CAMLparam5(window, position, bitboard, square, prev);
    CAMLlocal2(piece, sq_string);
    auto sf_window = Sfml_window_val(window);
    auto size = sf_window->getSize();
    int tw = size.x / 8;
    int th = size.y / 8;
    int sz = (tw + th) / 2;
    int sz2 = (tw + th) / 12;
    auto bb = Int64_val(bitboard);
    for (int y = 0; y < 8; ++y) {
      for (int x = 0; x < 8; ++x) {
        auto sq = Make_square(x, 7 - y);
        sf::RectangleShape tile(sf::Vector2f(tw, th));
        if (square != Val_none && Int_val(Some_val(square)) == sq) {
          tile.setFillColor(sf::Color(0xF3, 0xF7, 0x81));
        } else if (bb & (1ULL << sq)) {
          tile.setFillColor(sf::Color(0x2E, 0xCC, 0xFA));
        } else if (prev != Val_none &&
                   (Move_src(Some_val(prev)) == sq ||
                    Move_dst(Some_val(prev)) == sq)) {
          tile.setFillColor(sf::Color(0xCC, 0xCC, 0xCC));
        } else {
          if (y % 2) {
            if (x % 2) {
              tile.setFillColor(sf::Color(0xFF, 0xFF, 0xFF));
          } else {
              tile.setFillColor(sf::Color(0xF7, 0xF2, 0xF0));
            }
          } else {
            if (x % 2) {
              tile.setFillColor(sf::Color(0xF7, 0xF2, 0xF0));
            } else {
              tile.setFillColor(sf::Color(0xFF, 0xFF, 0xFF));
            }
          }
        }
        tile.setPosition(x * tw, y * th);
        sf_window->draw(tile);
        piece = caml_callback2(*caml_named_value("piece_at_square"),
                               position,
                               Val_int(sq));
        if (piece != Val_none) {
          auto unicode = piece_unicode(Piece_color(Some_val(piece)),
                                       Piece_kind(Some_val(piece)));
          sf::Text piece_text(unicode, _piece_font, sz);
          float fx = x * tw;
          float fy = y * th;
          float xa = ((float)(tw - sz) / 2.0f) + ((float)sz / 8.5f);
          float ya = ((float)(th - sz) / 10.0f) - ((float)sz / 5.0f);
          piece_text.setPosition(fx + xa, fy + ya);
          piece_text.setFillColor(sf::Color::Black);
          sf_window->draw(piece_text);
        }
        sq_string = caml_callback(*caml_named_value("string_of_square"),
                                  Val_int(sq));
        sf::Text tile_text(String_val(sq_string), _text_font, sz2);
        tile_text.setPosition(x * tw, y * th);
        tile_text.setFillColor(sf::Color(0x00, 0x00, 0x00, 0x5F));
        sf_window->draw(tile_text);
      }
    }
    CAMLreturn(Val_unit);
  }
}
