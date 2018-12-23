c     *** the mortran 2.0 processor of `75 modified by ajc july `87 ***
C     *** modified by jhf Feb 2016
C     *** modified by Naras to accept command line args
C     ****   m77.mac, mo.for and mortlist in that order
C     subroutine setup
C     character(LEN=256)  fname
C     i = 1
C     open (1,file='./m77.mac',status='old')
C     open (7,file='./mo.for', status='unknown')
C     open (8,file='./mortlist', status='unknown')
C     return
C     end
      PROGRAM MORTRAN
      implicit integer (a-z)
      dimension mm( 32 000 )
      character(LEN=256) fname
      common/mem/mm
      common/status/iq,q,ir,r,c,nc,h,mtrc,mdef,mcnt,ngen,iquo,mort ,merr
     1,uptr,unit,list,indt,note,ncrd
      logical cnv
      equivalence (lims,v),(limf,io),(limr,ic),(limq,ir)
      data ic/300/,ia/400/,is/401/,g/10000/,nscn/0/,limu/ 32 000 /
c
      call getarg(1, fname)
      open (1,file=trim(fname),status='old')
      call getarg(2, fname)
      open (7,file=trim(fname), status='unknown')
      call getarg(3, fname)
      open (8,file=trim(fname), status='unknown')

c
      read (1,20) (mm(k),k=1,152)
      write (8,30) (mm(k),k=1,72)
      write (6,30) (mm(k),k=1,72)
      write (7,10) (mm(k),k=1,72)
 10   format(1hc,72a1)
 20   format(80a1)
 30   format(1h1,72a1,12x,   32h processor version of 27 feb 78 /)
 40   format(1x,120a1)
      f=limu/8
      io=f*5
      ie=f*7
      mm(ia)=ia+25
      mm(ie)=mm(132)
      mm(ir)=0
      lima=f
      o=io
      u=ie
      s=ie
      v=ie
      k=f+14
      read (1,50) kq,ka,ip,(mm(j),j=f,k)
 50   format(40a2)
      j=ic
      mm(j)=mm(140)
      mm(j+1)=f
      mm(j+3)=-1
      mm(f)=0
      mm(f+1)=f+12
      mm(f+2)=f+14
      mm(f+3)=0
      f=f+15
      kilmac=f
 60   if (c.eq.nc) go to 170
 70   c=c+1
      if (c.gt.nc) call nxtcrd (mm(117))
 80   if (mm(c).ne.mm(135)) go to 110
 90   c=c+1
      if (c.gt.nc) call nxtcrd (mm(135))
      if (c.ne.nc) go to 100
      if (iquo.eq.1) go to 60
 100  if (mm(c).eq.mm(135)) go to 60
      go to 90
 110  u=u+1
      if (limu.le.u) u=u+nono(1,6,8,1)
      mm(u)=mm(c)
      if (mm(c).ne.mm(134)) go to 130
      mm(u)=kq
 120  c=c+1
      if (c.gt.nc) call nxtcrd (mm(134))
      u=u+1
      if (limu.le.u) u=u+nono(1,6,8,1)
      mm(u)=mm(c)
      if (mm(c).ne.mm(134)) go to 120
      c=c+1
      if (c.gt.nc) call nxtcrd (mm(134))
      if (mm(c).eq.mm(134)) go to 120
      mm(u)=kq
      go to 80
 130  if (mm(c).ne.mm(117)) go to 150
      if (s.eq.u) go to 140
      if (mm(u-1).eq.mm(117)) u=u-1
 140  if (c.eq.nc) go to 170
      c=c+1
      if (mm(c).ne.mm(117)) go to 80
      go to 140
 150  if (mm(c).eq.mm(132)) go to 170
      if (mm(130).ne.mm(c)) go to 160
      h=h+1
      go to 60
 160  if (mm(131).ne.mm(c)) go to 60
      h=h-1
      go to 60
 170  if (s.le.u) go to 180
      s=ie
      u=s-1
      go to 70
 180  if (mm(s).ne.mm(135)) go to 190
      nscn=1
      s=s+1
      go to 810
 190  m=ic
 200  if (mm(m).eq.mm(s)) go to 210
      m=m+2
      if (mm(m+1).eq.-1) go to 810
      go to 200
 210  m=m+1
 220  m=mm(m)
      if (m.eq.0) go to 810
      d=m+4
      b=mm(m+1)
      z=0
      e=0
      t=ia-1
      a=mm(ia)
      v=s
 230  if (v.le.u) go to 250
      if (s.ne.u) go to 240
      s=ie
      mm(s)=mm(u)
      u=s
 240  if (mort.eq.0) go to 220
      go to 70
 250  if (mm(d).ne.ip) go to 290
      d=d+1
      t=t+1
      mm(t)=a
      if (mm(d).ne.ka) go to 270
      d=d+1
      e=n1(mm(d),0,36,0)
      d=d+1
 260  if (e.le.0) go to 280
      if (v.gt.u) go to 70
      mm(a)=mm(v)
      a=a+1
      if (lima.le.a) a=a+nono(2,6,8,1)
      v=v+1
      e=e-1
      go to 260
 270  e=d
      w=v
      iz=z
      p=0
 280  if (b.le.d) go to 300
      go to 230
 290  if (mm(d).ne.mm(v)) go to 310
      if (mm(d).eq.kq) z=1-z
      d=d+1
      v=v+1
      if (b.gt.d) go to 230
 300  la=t
      mm(t+1)=a
      mm(t+2)=0
      e=mm(m+2)
      b=b-1
      s=o
      w=mm(m+3)
      mcnt=mcnt-1
      if (mcnt.le.0) mtrc=3+nono(3,15,16,0)
      if (mtrc.eq.2) call mactrc (1,m+4,b,ia,la)
      go to 430
 310  if (mm(v).ne.mm(117).or.z.ne.0) go to 320
      v=v+1
      go to 230
 320  if (e.eq.0) go to 220
      if (mm(t).ne.a.or.mm(w).ne.mm(117)) go to 330
      if (z.ne.0) go to 330
      w=w+1
      go to 230
 330  mm(a)=mm(w)
      a=a+1
      if (lima.le.a) a=a+nono(2,6,8,1)
      if (mm(w).ne.kq) go to 350
      if (z.eq.1.or.iz.eq.1) go to 220
 340  w=w+1
      mm(a)=mm(w)
      a=a+1
      if (lima.le.a) a=a+nono(2,6,8,1)
      if (w.gt.u) go to 220
      if (mm(w).eq.kq) go to 410
      go to 340
 350  if (z.ne.0) go to 400
      if (mm(120).ne.mm(w)) go to 360
      p=p+1
      go to 370
 360  if (mm(121).ne.mm(w)) go to 370
      p=p-1
 370  if (mm(130).ne.mm(w)) go to 380
      p=p+1
      go to 390
 380  if (mm(131).ne.mm(w)) go to 390
      p=p-1
 390  if (0.gt.p) go to 220
      if (mm(w).eq.mm(132)) go to 220
 400  if (mm(w).ne.ka) go to 410
      w=w+1
      mm(a)=mm(w)
      a=a+1
 410  w=w+1
      if (w.le.u) go to 420
      if (mort.eq.0) go to 220
      go to 70
 420  if (p.ne.0) go to 330
      d=e
      v=w
      z=iz
      go to 230
 430  b=b+1
      if (b.le.e) go to 470
      if (w.eq.00) go to 440
      b=w+1
      e=mm(b)
      w=mm(w)
      go to 430
 440  if (mtrc.eq.0) go to 450
      if (s.ne.o) call mactrc (2,o+1,s,0,0)
 450  if (s.ne.o) go to 460
      s=v
      go to 170
 460  v=v-1
      mm(v)=mm(s)
      s=s-1
      go to 450
 470  if (mm(b).ne.ip) go to 490
      b=b+1
      n=n1(mm(b),1,la-ia+1,1)+ia
      a=mm(n-1)
      n=mm(n)
 480  if (n.le.a) go to 430
      s=s+1
      mm(s)=mm(a)
      a=a+1
      go to 480
 490  s=s+1
      if (lims.le.s) s=s+nono(15,6,8,1)
      mm(s)=mm(b)
      if (mm(b).ne.ka) go to 430
      b=b+1
      if (mm(b).ne.mm(93)) go to 530
      b=b+1
      s=s-1
      if (mm(b).ne.mm(104)) go to 510
      if (ngen.ne.0) go to 500
      kgen=1
      ngen=1
      go to 430
 500  kgen=kgen+1
      go to 430
 510  if (mm(b).ne.mm(97)) go to 520
      if (ngen.eq.1) kgen=kgen+1
      go to 430
 520  if (mm(b).ne.mm(95)) go to 530
      kgen=kgen-1
      if (kgen.le.0) ngen=0
      go to 430
 530  if (ngen.ne.1) go to 540
      s=s-1
      go to 430
 540  if (mm(b).ne.mm(99)) go to 590
      b=b+1
      s=s-1
      if (mm(b).ne.mm(124)) go to 550
      knt=knt+1
      go to 430
 550  if (mm(b).ne.mm(125)) go to 560
      knt=knt-1
      go to 430
 560  if (mm(b).ne.mm(81)) go to 570
      knt=0
      go to 430
 570  if (mm(b).ne.mm(123)) go to 580
      knt=mm(s)
      s=s-2
      go to 430
 580  if (mm(b).ne.mm(93)) go to 590
      s=s+2
      mm(s)=knt
      go to 430
 590  if (mm(b).ne.mm(104)) go to 630
      b=b+1
      s=s-1
      if (mm(b).ne.mm(124)) go to 600
      h=h+1
      go to 430
 600  if (mm(b).ne.mm(125)) go to 610
      h=h-1
      go to 430
 610  if (mm(b).ne.mm(102)) go to 620
      s=s+2
      mm(s)=ncrd
      go to 430
 620  if (mm(b).ne.mm(103)) go to 630
      write (8,40) (mm(jj),jj=o,s)
      s=o
      go to 430
 630  if (mm(b).ne.mm(103)) go to 710
      b=b+1
      if (mm(b).ne.mm(109)) go to 640
      s=s-1
      b=b+1
      q=q+1
      if (limq.le.q) q=q+nono(3,7,8,0)
      mm(q)=mm(b)
      go to 430
 640  if (mm(b).ne.mm(108)) go to 650
      s=s-1
      b=b+1
      mm(q)=mm(b)
      go to 430
 650  if (mm(b).ne.mm(111)) go to 660
      mm(s)=mm(142)
      s=s+1
      if (q.eq.iq) q=q-nono(3,7,9,0)
      mm(s)=mm(q)
      q=q-1
      go to 430
 660  if (mm(b).ne.mm(110)) go to 670
      if (mtrc.eq.1) call mactrc (1,m+4,mm(m+1)-1,ia,la)
      s=s-1
      go to 430
 670  if (mm(b).ne.mm(97)) go to 680
      icat=0
      go to 930
 680  if (mm(b).ne.mm(93)) go to 690
      icat=1
      go to 930
 690  if (mm(b).ne.mm(94)) go to 700
      icat=2
      go to 930
 700  if (mm(b).ne.mm(101)) go to 710
      f=kilmac
      go to 230
 710  if (mm(b).ne.mm(102)) go to 800
      b=b+1
      if (mm(b).ne.mm(97)) go to 720
      g=g+10
      s=s+1
      mm(s)=g
      go to 430
 720  if (mm(b).ne.mm(110)) go to 730
      s=s-2
      call ncv (s,mm(s+1))
      s=s-1
      go to 430
 730  j=n1(mm(b+1),0,9,0)
      if (mm(b).ne.mm(93)) go to 740
      s=s+1
      k=r-j
      mm(s)=mm(k)+n1(mm(b+2),0,9,0)
      b=b+2
      go to 430
 740  if (mm(b).ne.mm(109)) go to 770
      r=r+1
      if (limr.le.r) r=r+nono(4,7,8,0)
      if (j.eq.0) go to 760
      do 750 k=1,j
      kk=r-k
      mm(kk+1)=mm(kk)
 750  continue
 760  k=r-j
      mm(k)=mm(s-1)
      b=b+1
      s=s-3
      go to 430
 770  if (mm(b).ne.mm(111)) go to 800
      k=r-ir
      if (j.gt.k) j=nono(4,7,9,0)
      if (j.le.0) go to 790
      do 780 k=1,j
      kk=r-j+k
      mm(kk-1)=mm(kk)
 780  continue
 790  if (0.le.j) r=r-1
      b=b+1
      s=s-1
      go to 430
 800  b=b-1
      go to 430
 810  if (mm(s).eq.mm(132)) then
        if (o.ne.io) call outlin (io,o)
      else
        if (mm(s).eq.ka.or.mm(s).eq.kq) then
          z=0
 830      if (mm(s).eq.ka) then
            s=s+1
            call ncv (o,mm(s))
            if (z.eq.0) go to 920
          else
            if (mm(s).eq.kq.or.s.gt.u) then
              if (ngen.eq.0) then
                mm(o)=mm(134)
                o=o+1
              endif
              if (z.eq.1) go to 920
              z=1
            else
              if (ngen.eq.0) then
                if (mm(s).eq.mm(134)) then
                   mm(o)=mm(134)
                   o=o+1
                 endif
                 mm(o)=mm(s)
                 o=o+1
                endif
            endif
          endif
           s=s+1
           go to 830
        else
          if (mm(s).eq.mm(135)) then
            nscn=0
            s=s+1
            go to 170
           endif
           if (ngen.eq.0) then
              if (mm(s).ne.mm(117).or.o.ne.io) then
                mm(o)=mm(s)
                o=o+1
              endif
           endif
        endif
      endif
 920  s=s+1
      if (nscn.eq.0) go to 170
      if (s.gt.u) go to 170
      go to 810
 930  t=ia
      s=s-1
      if (mm(t+1).ne.mm(t)) go to 940
      i=nono(12,3,14,0)
      go to 430
 940  a=mm(t)
      t=t+1
      n=a-1
      z=0
 950  if (mm(t).le.a) go to 1030
      if (mm(a).ne.mm(134)) go to 970
      if (z.ne.0) go to 960
      mm(a)=kq
      z=1
      go to 1010
 960  a=a+1
      if (mm(a).eq.mm(134).and.mm(t).gt.a) go to 1010
      a=a-1
      mm(a)=kq
      z=0
      go to 1010
 970  if (mm(a).ne.mm(141)) go to 990
      if (mm(a+1).ne.mm(141)) go to 980
      a=a+1
      go to 1010
 980  mm(a)=ip
      go to 1010
 990  if (mm(a).ne.mm(142)) go to 1010
      if (mm(a+1).ne.mm(142)) go to 1000
      a=a+1
      go to 1010
 1000 mm(a)=ka
 1010 n=n+1
      mm(n)=mm(a)
      if (mm(a).eq.mm(117).and.z.ne.1) go to 1020
      a=a+1
      go to 950
 1020 a=a+1
      if (mm(t).le.a.or.mm(a).ne.mm(117)) go to 950
      go to 1020
 1030 if (t.ne.is) go to 1040
      i1=n
      if (icat.eq.2) go to 1050
      go to 940
 1040 i2=n
      go to 1050
 1050 if (icat.ne.0) go to 1140
 1060 if (icat.eq.2) go to 430
      n=f
      f=f+4
      a=mm(ia)
 1070 if (a.gt.i1) go to 1080
      mm(f)=mm(a)
      f=f+1
      if (limf.le.f) f=f+nono(3,6,8,1)
      a=a+1
      go to 1070
 1080 mm(n+1)=f
      a=mm(is)
 1090 if (a.gt.i2) go to 1100
      mm(f)=mm(a)
      f=f+1
      if (limf.le.f) f=f+nono(3,6,8,1)
      a=a+1
      go to 1090
 1100 mm(n+2)=f-1
      mm(n+3)=0
      t=ic
      a=mm(ia)
 1110 if (mm(a).ne.mm(t)) go to 1120
      mm(n)=mm(t+1)
      go to 1130
 1120 t=t+2
      if (mm(t+1).ne.-1) go to 1110
      mm(t)=mm(a)
      mm(t+3)=-1
      mm(n)=0
      go to 1130
 1130 mm(t+1)=n
      t=3
      go to 1270
 1140 t=ic
      a=mm(ia)
 1150 if (mm(t).eq.mm(a)) go to 1160
      t=t+2
      if (mm(t+1).eq.-1) go to 1060
      go to 1150
 1160 i=t+1
      t=mm(i)
      n=t
 1170 t=n+4
      a=mm(ia)
      l1=mm(n+1)-t
      l2=mm(is)-a
      if (l1.ne.l2) go to 1190
 1180 if (mm(t).ne.mm(a)) go to 1190
      t=t+1
      a=a+1
      if (a.gt.i1) go to 1200
      go to 1180
 1190 i=n
      n=mm(n)
      if (n.eq.0) go to 1060
      go to 1170
 1200 if (icat.ne.1) go to 1260
      t=n+3
 1210 if (mm(t).eq.00) go to 1220
      t=mm(t)
      go to 1210
 1220 mm(t)=f
      go to 1230
 1230 a=mm(is)
      mm(f)=0
      n=f+1
      f=f+2
 1240 if (a.gt.i2) go to 1250
      mm(f)=mm(a)
      f=f+1
      if (limf.le.f) f=f+nono(3,6,8,1)
      a=a+1
      go to 1240
 1250 mm(n)=f-1
      t=4
      go to 1270
 1260 mm(i)=mm(n)
 1270 if (mdef.gt.0) call mactrc (t,mm(ia),i1,mm(is),i2)
      go to 430
      end
      block data
      implicit integer (a-z)
      common/status/ iq, q, ir, r, c, nc, h,
     1mtrc, mdef, mcnt, ngen, iquo, mort, merr,
     2uptr, unit, list, indt, note, ncrd
c
      data iq/200/,q/200/,ir/250/,r/250/,c/80/,nc/72/,h/0/,
     1mtrc/0/, mdef/0/, mcnt/500/, ngen/0/, iquo/0/, mort/1/, merr/0/,
     2uptr/1/, unit/1/, list/0/,   indt/0/, note/0/, ncrd/0/
      end
      subroutine mactrc (w,i,j,k,l)
      implicit integer (a-z)
      dimension mm( 32 000 )
      common/mem/mm
      go to (10,30,40,50),w
 10   write (8,70) (mm(jj),jj=i,j)
      if (k.gt.l) go to 60
      do 20 t=k,l
      m=mm(t)
      n=mm(t+1)-1
      a=t-k+1
      if (m.le.n) write (8,80) a,(mm(jj),jj=m,n)
 20   continue
      go to 60
 30   write (8,90) (mm(jj),jj=i,j)
      go to 60
 40   write (8,100) (mm(jj),jj=i,j)
      write (8,110) (mm(jj),jj=k,l)
      go to 60
 50   write (8,100) (mm(jj),jj=i,j)
      write (8,120) (mm(jj),jj=k,l)
 60   return
 70   format(   31h             pattern matched is ,80a1/(30x,80a1) )
 80   format(   22h              argument,i3,    3h is ,80a1/(30x,80a1)
     1)
 90   format(   31h                   expansion is ,80a1/(30x,80a1) )
 100  format(   31h   macro definition, pattern is ,80a1/(30x,80a1) )
 110  format(   31h                 replacement is ,80a1/(30x,80a1) )
 120  format(   31h           catenation string is ,80a1/(30x,80a1) )
      end
      subroutine nxtcrd (m)
      implicit integer (a-z)
      dimension mm( 32 000 )
      common/mem/mm
      common/status/iq,q,ir,r,c,nc,h,mtrc,mdef,mcnt,ngen,iquo,mort ,merr
     1,uptr,unit,list,indt,note,ncrd
      logical cnv
      integer ustk,uptr,unit
      dimension ustk(20),i80(80),i79(79)
      equivalence (mm(1),i79(1),i80(1) ) , (mm(180),ustk(1) )
      mcnt=500
      if (uptr.ne.0) go to 110
      if (q.eq.iq) go to 20
      merr=merr+1
      write (8,10)
      write (6,10)
 10   format (   17h ******** error -,   23h unclosed loop or block)
 20   if (r.eq.ir) go to 40
      merr=merr+1
      write (8,30)
      write (6,30)
 30   format (   17h ******** error -,   22h missing right bracket)
 40   if (m.ne.mm(134)) go to 60
      merr=merr+1
      write (8,50)
      write (6,50)
 50   format (   17h ******** error -,   16h unclosed string)
 60   if (m.ne.mm(135)) go to 80
      merr=merr+1
      write (8,70)
      write (6,70)
 70   format (   17h ******** error -,   17h unclosed comment)
 80   write (8,90) merr
      write (6,90) merr
 90   format(i5,   27h mortran errors encountered)
      if (merr.eq.0) stop
      write (7,100)
 100  format(6x,   23h****** mortran error(s) /6x,    3hend)
      stop 12
 110  read (unit,120) i80
      ncrd=ncrd+1
 120  format(80a1)
      if (list.eq.0) go to 180
      if (mort.ne.0) go to 140
      write (8,130) i80
 130  format(14x,80a1)
      go to 180
 140  k=max0(indt*h+1,1)
      l=1
      if (indt.eq.0) go to 160
 150  if (mm(l).ne.mm(117)) go to 160
      l=l+1
      if (nc.gt.l) go to 150
      l=1
      go to 160
 160  igen=mm(117)
      if (ngen.ne.0) igen=mm(114)
      write (8,170) ncrd,igen,m,h,(mm(117),i=1,k),(mm(i),i=l,80)
 170  format(1x,i4,1x,2a1,i3,2x,120a1/(7x,124a1) )
 180  if (mort.ne.0) go to 190
      if (mm(1).eq.mm(140)) go to 230
      write (7,120) i80
      go to 230
 190  if (note.ne.1) go to 210
      write (7,200) i79
      if (mm(80).ne.mm(117)) write (7,200) mm(80)
 200  format(1hc,79a1)
 210  if (note.ne.2) go to 230
      write (7,220) i80
 220  format(1hc,39x,40a1)
 230  if (mm(1).ne.mm(140)) go to 390
      if (mm(2).ne.mm(107)) go to 240
      iquo=n2(0,1,0)
      go to 110
 240  if (mm(2).ne.mm(110)) go to 250
      mtrc=n2(0,2,0)
      go to 110
 250  if (mm(2).ne.mm(94)) go to 260
      mdef=n2(0,2,0)
      go to 110
 260  if (mm(2).ne.mm(95)) go to 280
      write (8,270)
 270  format(1h1)
      go to 110
 280  if (mm(2).ne.mm(102)) go to 290
      list=1
      go to 110
 290  if (mm(2).ne.mm(104)) go to 300
      list=0
      go to 110
 300  if (mm(2).eq.mm(96)) go to 380
      if (mm(2).ne.mm(103)) go to 310
      mort=1
      c=nc
      mm(c)=mm(132)
      return
 310  if (mm(2).ne.mm(99)) go to 320
      indt=n2(0,50,2)
      go to 110
 320  if (mm(2).ne.mm(91)) go to 330
      note=n2(0,2,1)
      go to 110
 330  if (mm(2).ne.mm(93)) go to 340
      nc=n2(10,80,72)
      go to 110
 340  if (mm(2).ne.mm(108)) go to 350
      i=n2(1,99,8)
      rewind i
      go to 110
 350  if (mm(2).ne.mm(112)) go to 360
      kv=1
      go to 110
 360  if (mm(2).ne.mm(111)) go to 370
      ustk(uptr)=unit
      uptr=uptr+1
      unit=n2(1,99,unit)
      go to 110
 370  if (mm(2).ne.mm(140)) go to 390
      uptr=uptr-1
      if (uptr.eq.0) go to 380
      unit=ustk(uptr)
      go to 110
 380  mort=0
      c=nc-1
      mm(c)=mm(132)
      mm(nc)=mm(132)
      return
 390  if (mort.eq.0) go to 110
      c=1
      return
      end
      subroutine outlin (a,z)
      implicit integer (a-z)
      dimension mm( 32 000 )
      common/mem/mm
      common/status/iq,q,ir,r,c,nc,h,mtrc,mdef,mcnt,ngen,iquo,mort ,merr
     1,uptr,unit,list,indt,note,ncrd
      logical cnv
      dimension l(5)
      mcnt=500
      if (ngen.ne.0) return
      i=1
      b=a
      y=z-1
      do 10 j=1,5
      l(j)=mm(117)
 10   continue
 20   if (mm(b).eq.mm(117)) go to 30
      if (cnv(mm(b),j,0,9)) go to 40
      l(i)=mm(b)
      i=i+1
 30   b=b+1
      if (i.gt.5.or.b.gt.y) go to 40
      go to 20
 40   j=65-y+b
      if (j.le.0) go to 50
      write (7,80) l,(mm(i),i=b,y),(mm(117),i=1,j),ncrd
      go to 70
 50   if (j.ne.0) go to 60
      write (7,80) l,(mm(i),i=b,y),ncrd
      go to 70
 60   j=b+65
      k=j+1
      write (7,80) l,(mm(i),i=b,j),ncrd,mm(117),(mm(i),i=k,y)
 70   z=a
      return
 80   format(5a1,1x,66a1,i7,a1,/(5x,    1h*,66a1) )
      end
      subroutine ncv (p,n)
      implicit integer (a-z)
      dimension mm( 32 000 )
      common/mem/mm
      common/status/iq,q,ir,r,c,nc,h,mtrc,mdef,mcnt,ngen,iquo,mort ,merr
     1,uptr,unit,list,indt,note,ncrd
      if (ngen.ne.0) return
      i=n
      p=p+5
      do 20 j=1,5
      k=p-j
      mm(k)=mm(117)
      t=i/10
      if (i.eq.0.and.j.ne.1) go to 10
      i=i-t*10+81
      mm(k)=mm(i)
 10   i=t
 20   continue
      return
      end
      logical function cnv(i,o,l,m)
      implicit integer (a-z)
      dimension mm( 32 000 )
      common/mem/mm
      cnv=.true.
      if (l.gt.m) return
      kl=l+81
      km=m+81
      do 10 k=kl,km
      if (mm(k).ne.i) go to 10
      o=k-81
      cnv=.false.
      go to 20
 10   continue
 20   return
      end
      function n1 (i,l,m,k)
      implicit integer (a-z)
      dimension mm( 32 000 )
      common/mem/mm
      common/status/iq,q,ir,r,c,nc,h,mtrc,mdef,mcnt,ngen,iquo,mort ,merr
     1,uptr,unit,list,indt,note,ncrd
      logical cnv
      n1=k
      n=k
      if (cnv(i,n,l,m)) go to 10
      n1=n
      return
 10   merr=merr+1
      write (8,20) i
      write (6,20) i
      return
 20   format(   38h ******** error - bad parameter number,1x,a2)
      end
      function n2 (l,m,k)
      implicit integer (a-z)
      dimension mm( 32 000 )
      common/mem/mm
      common/status/iq,q,ir,r,c,nc,h,mtrc,mdef,mcnt,ngen,iquo,mort ,merr
     1,uptr,unit,list,indt,note,ncrd
      logical cnv
      n2=-1
      if (mm(3).eq.mm(117)) go to 10
      if (cnv(mm(3),n1,0,9)) go to 30
      n2=n1
 10   if (mm(4).eq.mm(117)) go to 20
      if (cnv(mm(4),n1,0,9)) go to 30
      n2=10*n2+n1
 20   if (l.le.n2.and.n2.le.m) go to 50
 30   n2=k
      write (8,40) k
      write (6,40) k
      merr=merr+1
 40   format(' **** error -  control card: ',4a1, 8h assumed)
 50   return
      end
      function nono (i,j,k,l)
      implicit integer (a-z)
      dimension mm( 32 000 )
              common/mem/mm
      common/status/iq,q,ir,r,c,nc,h,mtrc,mdef,mcnt,ngen,iquo,mort ,merr
     1              ,uptr,unit,list,indt,note,ncrd
      character*8 a(16)
c
      data a(1) / 'input   ' /, a(2)  / 'paramter' /
      data a(3) / 'macro   ' /, a(4)  / 'label   ' /
      data a(5) / 'output  ' /, a(6)  / 'buffer  ' /
      data a(7) / 'stack   ' /, a(8)  / 'overflow' /
      data a(9) / 'undrflow' /, a(10) / 'digit   ' /
      data a(11)/ 'undefind' / ,a(12) / 'illegal ' /
      data a(13)/ 'string  ' / ,a(14) / 'pattern ' /
      data a(15)/ 'expnsion' / ,a(16) / 'loop    ' /
c
      write (8,30) a(i),a(j),a(k)
      write (6,30) a(i),a(j),a(k)
      nono=-1
      merr=merr+1
      if (l.ne.0) go to 10
      write (8,40)
      write (6,40)
      return
 10   write (7,20)
 20   format(6x,'****** mortran error(s), check listing'/6x,'end')
      stop 16
 30   format(' ******** error - ',3(1x,a) )
 40   format(' mortran processing continuing')
      end
      

