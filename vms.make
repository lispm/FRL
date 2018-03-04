#
#		f r l
#
# Installation notes:
#   * remove all lines with the word sticky (there are two). This command
#     only exists at UCB.  If you are a superuser, you can set the sticky
#     bit with the chmod command.

FRL = vmsfrl

# some files for frl stored in the lisp library, LLIB
LLIB= /usr/lib/lisp
# LLIB= /u0/csam/douglas/src/franz/lib/lisp

MACROS= frllib/fauxfns.l 

MOBJS=	frllib/fauxfns.o

LIBFILES = ${LLIB}/auxfns0.l   ${LLIB}/auxfns1.l  ${LLIB}/toplevel.l \
	   ${LLIB}/backquote.l	${LLIB}/machacks.l ${LLIB}/step.l \
	   ${LLIB}/fix.l ${LLIB}/trace.l

EXECUTIVE = Makefile doit.l init.l declar.l ldeclar.l Readme Setup \
	dhl/ReadMe	dhl/rule.doc	dhl/domain.doc


FRLFILES=	frl/faccess.l	frl/fask.l	frl/fassert.l	frl/fdump.l\
		frl/fherit.l	frl/flist.l	frl/fmacro.l	frl/freq.l\
		frl/futil.l	frl/thing.l	frl/thing1.l\
		frl/ttyio.l	frl/raccess.l	frllib/ftop.l

OILFILES=	oil/demo.l	oil/foil.l	oil/oil.l\
		oil/olddemo.l	

RULEFILES=	rule/rtemp.l	rule/rule.l	rule/sentin.l

TALKFILES=	talk/frmish.l	talk/ftalk0.l	\
		talk/pidgin.l	talk/rulish.l	

#
# other talk files not loaded in yet with talk-load
# talk/menu.l talk/names.l	\
# talk/say.l
# talk/shpish.l	talk/timish.l	
#

UTILFILES=	frl/util/cntrl.l\
		frl/util/ftrace.l	frl/util/set.l	frl/util/sutil.l\
		frl/util/util.l

FREDFILES=	frl/fred.l

DHLFILES=	dhl/rule.l	\
		dhl/rframes.l	dhl/domain.l \
		dhl/satisfy.l

FILES = $(MACROS) $(FRLFILES) \
	$(RULEFILES) $(UTILFILES) $(TALKFILES) $(EXECUTIVE) \
	${DHLFILES} utils/uctolc.c utils/ltags
# dont distribute  ${OILFILES} $(LIBFILES) 

LIBOBJS = ${LLIB}/auxfns0.o ${LLIB}/auxfns1.o ${LLIB}/toplevel.o \
	${LLIB}/machacks.o	${LLIB}/backquote.o \
	${LLIB}/step.o 	${LLIB}/fix.o	${LLIB}/trace.o

FRLOBJECTS=	frl/faccess.o	frl/fask.o	frl/fassert.o	frl/fdump.o\
		frl/fherit.o	frl/flist.o	frl/fmacro.o	frl/freq.o\
		frl/futil.o	frl/thing.o	frl/thing1.o\
		frl/ttyio.o	frl/raccess.o	frllib/ftop.o

OILOBJECTS=	oil/demo.o	oil/foil.o	oil/oil.o\
		oil/olddemo.o	

RULEOBJECTS=	rule/rtemp.o	rule/rule.o	rule/sentin.o

TALKOBJECTS=	talk/frmish.o	talk/ftalk0.o	\
		talk/pidgin.o	talk/rulish.o

UTILOBJECTS=	frl/util/cntrl.o\
		frl/util/ftrace.o	frl/util/set.o	frl/util/sutil.o\
		frl/util/util.o

FREDOBJECTS=	fred.o

DHLOBJECTS=	dhl/rule.o 	\
		dhl/rframes.o	dhl/domain.o \
		dhl/satisfy.o

OBJECTS =	$(MOBJS)	$(FRLOBJECTS)	\
		$(UTILOBJECTS)	$(RULEOBJECTS)	${DHLOBJECTS} 
# ${OILOBJECTS} ${LIBOBJS}

ALLOBJECTS =	$(OBJECTS)	$(TALKOBJECTS)	$(LIBOBJS)

TMP=/usr/tmp/
#
# setting for vms lblh machine: (reset for local lisp system).
CMPLR = csh -c "/rrc3/working/lisp/mac/lisp -r /rrc3/working/lisp/mac/liszt
LISP = /rrc3/working/lisp/mac/lisp -r /rrc3/working/lisp/mac/lisp
PROF= -x
.SUFFIXES: .l
#setting for vms machine.
.l.o:
	rm -f $*.o
	${CMPLR} $< -mq  ${PROF} " >> 'errs' 2>&1 

#
# frl
#
all:	$(FRL) ${TALKOBJECTS}
	@echo FRL and talkfiles compiled.

vmsfrl:	$(OBJECTS) $(EXECUTIVE)
	rm -f $(FRL)
	( (cat doit.l;echo "(savelisp 'vmsfrl)") | ${CMPLR} ) >> 'errs' 2>&1
	@echo FRL is compiled.
	
talkobjects: $(TALKOBJECTS)
	@echo talkobjects are compiled.


# generate a cross reference listing of all frl files.  
# This requires a lisp system with a large maximum size.
xref:	
	@echo	"lxref <lots of files>"
	@lxref ${FILES} > xref

tarbaby: 
	tar cv Makefile ${FILES} $(FRL)
	@echo tar done

tarobjtape:
	tar cv ${ALLOBJECTS} ${EXECUTIVE} $(FRL)
	@echo tar done

#	command local to lbl to send files through uucp to the vms machine.

sendtoh:
	@csh /u0/csam/douglas/bin/sendtoh $(FILES)
	@echo files queued.
	
movedir: 
	tar cf - ${FILES} ${ALLOBJECTS} $(FRL) | \
	(cd /u0/csam/steve/rosenberg; tar xfv -)
	@echo tar done


${LLIB}/auxfns0.o: ${LLIB}/auxfns0.l
	${CMPLR} ${PROF} ${LLIB}/auxfns0 > ${LLIB}/auxfns0.blat 2>&1

${LLIB}/auxfns1.o: ${LLIB}/auxfns1.l
	${CMPLR} ${PROF} ${LLIB}/auxfns1 > ${LLIB}/auxfns0.blat 2>&1

${LLIB}/toplevel.o: ${LLIB}/toplevel.l
	${CMPLR} ${PROF} ${LLIB}/toplevel > ${LLIB}/toplevel.blat 2>&1


clean:
	@echo removing all objects and FRL
	@rm -f $(OBJECTS) $(TALKOBJECTS) $(FRL)

tags:	utils/uctolc $(FILES)
	@echo making tags file
	@awk -f utils/ltags ${FILES} | utils/uctolc | sort > tags
	@echo tags file created

utils/uctolc: utils/uctolc.c
	cc -o utils/uctolc utils/uctolc.c

filelist:
	@echo $(FILES)

echo:
	echo ${ALLOBJECTS}

atape:
	tar crfb /dev/rmt0 20 .


