# Makefile

FPC=fpc
SRCDIRS=. utils utils/bgrabitmap5.7.1 ontheair game tests
LCLSRC=/usr/lib/lazarus/lcl/units/x86_64-linux
GUIFLAVOR=gtk2
FPCOPT=-MObjFPC -Schi -O1 -gl -vewnhi -l
LIBDIR=lib/x86_64-linux
EXECUTABLE=pinball

all: $(EXECUTABLE)

$(EXECUTABLE):
	$(FPC) $(FPCOPT) -Fi$(LIBDIR) `echo $(SRCDIRS) | xargs -n1 printf "-Fu%s "` -Fu$(LCLSRC) -Fu$(LCLSRC)/$(GUIFLAVOR) -Fu/usr/lib/lazarus/packager/units/x86_64-linux -FU$(LIBDIR) -o$(EXECUTABLE) -dLCL -dLCL$(GUIFLAVOR) pinball.lpr

cleanbak:
	find . -iname '*.bak' -type f -delete

clean:
	rm $(EXECUTABLE)

purge: clean cleanbak
	find . -iname '*.o' -type f -delete
	find . -iname '*.ppu' -type f -delete

.PHONY: $(EXECUTABLE) clean purge cleanbak
