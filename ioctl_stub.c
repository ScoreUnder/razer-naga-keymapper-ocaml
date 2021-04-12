#include <linux/input.h>
#include <sys/ioctl.h>
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

CAMLprim value caml_eviocgrab(value fd)
{
	return Val_int(ioctl(Int_val(fd), EVIOCGRAB, 1));
}
