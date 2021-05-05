#include <X11/Xlib.h>
#include <X11/extensions/XTest.h>
#include <stdio.h>
#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

CAMLprim value caml_xtest_fake_button(value display, value button, value press) {
    CAMLparam3(display, button, press);
    Display *dpy = *(Display**) Data_custom_val(display);
    int result = XTestFakeButtonEvent(dpy, Int_val(button), Int_val(press), CurrentTime);
    CAMLreturn(Val_int(result));
}

CAMLprim value caml_xtest_fake_key(value display, value key, value press) {
    CAMLparam3(display, key, press);
    Display *dpy = *(Display**) Data_custom_val(display);
    int result = XTestFakeKeyEvent(dpy, Int_val(key), Int_val(press), CurrentTime);
    CAMLreturn(Val_int(result));
}

CAMLprim value caml_string_to_keysym(value string) {
    CAMLparam1(string);
    CAMLreturn(Val_int(XStringToKeysym(String_val(string))));
}

CAMLprim value caml_keysym_to_keycode(value display, value sym) {
    CAMLparam2(display, sym);
    Display *dpy = *(Display**) Data_custom_val(display);
    CAMLreturn(Val_int(XKeysymToKeycode(dpy, Int_val(sym))));
}

CAMLprim value caml_xflush(value display) {
    CAMLparam1(display);
    Display *dpy = *(Display**) Data_custom_val(display);
    int result = XFlush(dpy);
    CAMLreturn(Val_int(result));
}

CAMLprim void caml_xclose_display(value display) {
    CAMLparam1(display);
    Display *dpy = *(Display**) Data_custom_val(display);
    XCloseDisplay(dpy);
    CAMLreturn0;
}

struct custom_operations x_display_custom = {
    .identifier = "x_display_custom",
    .finalize = caml_xclose_display,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .fixed_length = custom_fixed_length_default,
};

CAMLprim value caml_xopen_display() {
    CAMLparam0();
    CAMLlocal2(result, dpy_r);
    Display *dpy = XOpenDisplay(NULL);
    if (dpy == NULL) {
        result = Val_none;
    } else {
        // NOLINTNEXTLINE(bugprone-sizeof-expression)
        dpy_r = caml_alloc_custom(&x_display_custom, sizeof dpy, 0, 1);
        *(Display**) Data_custom_val(dpy_r) = dpy;
        result = caml_alloc_some(dpy_r);
    }
    
    CAMLreturn(result);
}
