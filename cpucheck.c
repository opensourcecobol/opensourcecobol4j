#include <stdio.h>
#include <string.h>

int
main ()
{
#ifdef __i386__
#if ( __GNUC__ == 3 && __GNUC_MINOR__ > 0 ) || __GNUC__ > 3
#if __GNUC__ > 3 || __GNUC_MINOR__ > 3
	char *ctune = "-mtune=";
#else
	char *ctune = "-mcpu=";
#endif
	char vendor_string[16];
	int eax, ebx, edx, ecx;
	int i, hv;
	int family, model, stepping;

	__asm__ (".byte 0x0f,0xa2"
	: "=a" (hv), "=b" (ebx), "=d" (edx), "=c" (ecx) : "0" (0));

	*(int *) (vendor_string + 0) = ebx;
	*(int *) (vendor_string + 4) = edx;
	*(int *) (vendor_string + 8) = ecx;
	vendor_string[12] = 0;
	i = 1;

	__asm__ (".byte 0x0f,0xa2"
	: "=a" (eax), "=b" (ebx), "=d" (edx), "=c" (ecx) : "0" (i));

	family = (eax >> 8) & 0xf;
	model = (eax >> 4) & 0xf;
	stepping = eax & 0xf;
	if (family == 0xf) {
		/* "extended" mode. */
		family += (eax >> 20) & 0xff;
		model += (eax >> 12) & 0xf0;
	}

	if (strcmp (vendor_string, "GenuineIntel") == 0) {
		if (family == 5) {
			printf ("-march=i686 %spentium", ctune);
		} else if (family == 6) {
			if (model <= 2) {
				printf ("-march=i686 %spentiumpro",ctune);
			} else if (model >= 3 && model <= 6) {
				printf ("-march=i686 %spentium2",ctune);
			} else if (model <= 11) {
				printf ("-march=i686 %spentium3", ctune);
			} else {
				printf ("-march=i686 %spentium4", ctune);
			}
		}
		else if (family == 15) {
			printf ("-march=i686 %spentium4", ctune);
		}
	} else if (strcmp (vendor_string, "AuthenticAMD") == 0) {
		if (family == 6) {
			printf ("-march=i686 %sathlon", ctune);
		}
	}
#endif
#endif
	return 0;
}
