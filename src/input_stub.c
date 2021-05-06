#include <unistd.h>
#include <linux/input.h>
#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

static value unknown_event(struct input_event ev)
{
	value v = caml_alloc_small(3, 0);
	Field(v, 0) = Val_int(ev.type);
	Field(v, 1) = Val_int(ev.code);
	Field(v, 2) = Val_int(ev.value);
	return v;
}

static int tag_types[] = {[EV_REL]=2, [EV_ABS]=3, [EV_MSC]=4};

CAMLprim value caml_read_some_input_events(value fdv)
{
	CAMLparam1(fdv);
	CAMLlocal4(list, next, result, evtype);

	struct input_event out[8];
	int fd = Int_val(fdv);

	caml_release_runtime_system();
	ssize_t size = read(fd, &out, sizeof(out));
	caml_acquire_runtime_system();
	if (size == 0) {
		caml_raise_end_of_file();
	} else if (size == -1) {
		uerror("read", caml_copy_string(""));
	} else if (size % sizeof(out[0]) != 0) {
		caml_failwith("partial read of input event");
	}
	size /= sizeof(out[0]);
	list = Val_emptylist;

	for (int n = size - 1; n >= 0; n--) {
		switch (out[n].type) {
			case EV_SYN:
				evtype = Val_int(0);
				break;
			case EV_KEY:
				if (out[n].value >= 0 && out[n].value <= 2) {
					evtype = caml_alloc_small(2, 1);
					Field(evtype, 0) = Val_int(out[n].code);
					Field(evtype, 1) = Val_int(out[n].value);
				} else {
					evtype = unknown_event(out[n]);
				}
				break;
			case EV_REL:
			case EV_ABS:
			case EV_MSC:
				evtype = caml_alloc_small(2, tag_types[out[n].type]);
				Field(evtype, 0) = Val_int(out[n].code);
				Field(evtype, 1) = Val_int(out[n].value);
				break;
			default:
				evtype = unknown_event(out[n]);
		}

		result = caml_alloc_small(3, 0);
		Field(result, 0) = Val_long(out[n].input_event_sec);
		Field(result, 1) = Val_long(out[n].input_event_usec);
		Field(result, 2) = evtype;

		next = caml_alloc_small(2, 0);
		Field(next, 0) = result;
		Field(next, 1) = list;
		list = next;
	}

	CAMLreturn(list);
}
