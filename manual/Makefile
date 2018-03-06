frl.pdf: frl.ps
	ps2pdf frl.ps

frl.ps: frl.n
	echo ".Ib" > frl.index
	groff -fT -mpapersize -dpaper=a4 -P-pa4 -me -w mac -step -mlmacs -M. frl.n >frl.ps 2>>frl.index
	sort +3 frl.index | grep -v '^troff' > frl.bindex
	cat frl.n frl.bindex |groff -fT -mpapersize -dpaper=a4 -P-pa4 -me -w mac -step -mlmacs -M. >frl.ps

clean:
	rm frl.bindex
	rm frl.ps
	rm frl.pdf
