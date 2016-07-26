From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 14:03:10 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA08034; Thu, 2 Jun 88 14:03:09 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA04233; Thu, 2 Jun 88 13:29:54 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA08239; Thu, 2 Jun 88 12:47:16+0900
Date: Thu, 2 Jun 88 12:47:16+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083>
Message-Id: <8806020347.AA08239@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: uctolc.c.frl
Status: RO


#include <stdio.h>
#include <ctype.h>
/*   uctolc   					j foderaro
	convert upper case to lower case
	in the first field of a tags file
 */
#define TRUE -1
main()
{
	register char c;
	while(TRUE)
	{
	  pt1:
		while( (c=getchar()) != EOF)
		{	
		    if(isupper(c))putchar( c + ('a' - 'A'));
		    else if(c == ' ') goto pt2;
		    else putchar(c);
		}
	        exit();
	pt2:
		putchar(c);
		while( (c=getchar()) != EOF)
		{
		    putchar(c);
		    if(c == '\n') goto pt1;
		}
		exit();
	}
}


