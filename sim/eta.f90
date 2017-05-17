module input_module_3d
implicit none
 
!Parameters
integer,parameter :: L=18
double precision, parameter :: pi=3.14159265358979323846264338327
double precision, parameter :: T=0,Hmax1=0,Hmin1=0,CON=0.00000001
double precision, parameter :: etamin=0,etamax=3
integer,parameter    :: nfield1=1, ntrans =1, nmeas = 1,nsite=L*L*L,intervals=10,etasteps=30
integer,parameter :: degauss=0
       
       
!Data types
double precision :: E,mav,mav2,mx,my,mz,Eav2,Eav,r,ct,phi,st,m1,m2,dh,x,mzav,mxav,myav,eav4,UE,hamil,jex,di
INTEGER ::TABLE(L,L,L),ITABLE(L*L*L,3),sm(L*L*L),isub(L*L*L),nbr(L*L*L,12)
integer :: nmax,IS,m,it,ix,iyy,iz,ns,ixp,ixm,iyyp,iyym,izp,izm
double precision :: r250_,theta
double precision :: R2,cp,sp,s1,s2,rr,CVT,dum,thet,u1,u2,u3,rnscx,rnscy,rnscz,sdn,rnd
double precision :: hpx1,hpy1,hpz1,hmp,hm,hmi,hx,hy,hz,sth,sthi,cph,sph,y,m0z,m0zav,&
    & m0x,m0xav,m0y,m0yav,sxtmp,sytmp,sztmp,stnew
double precision ::  sx(L*L*L),sy(L*L*L),sz(L*L*L),D(3,3,0:L-1,0:L-1,0:L-1),s(L*L*L,3),nx(L*L*L),ny(L*L*L),nz(L*L*L)
double precision :: smx(0:3),smy(0:3),smz(0:3),sm2av(0:3),smav(0:3),s0x(L*L*L),s0y(L*L*L),s0z(L*L*L),smxalt(0:3),smyalt(0:3)
double precision :: rmx,rmy,rmz,rnum,rmav,rmav2,rmxav,rmyav,rmzav,rm2root,rmav4,smzalt(0:3),sm2avalt(0:3),smavalt(0:3)
double precision :: UB,UB2,nxsum,nysum,nzsum,ent,entav,beta1,beta2,t1,t2,nbrx(L*L*L),nbry(L*L*L),nbrz(L*L*L),Hmag,hmx,hmy,hmz
double precision :: mcx,mcy,mcz,msx,msy,msz,chi,cm2av,cmxav,cmyav,cmzav,mcav,mcav2,msav,msav2,nocc
double precision :: Htheta,Hphi
double precision :: Ecurr,Eprev,tol,garbtmp
double precision :: eta0
integer :: imeas,irand0,iseed,ran
integer :: flush
integer :: i,j,k,dx,dy,dz,ax,ay,az,bx,by,bz,cx,cy,cz,i2,i4,i5,i11,i12
integer :: a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3
integer :: nz0,nz1,nz2,nz3
integer :: counter,duration,fieldsteps

external r250_, flush
character(20):: conffile
common /table1/ Table,itable,D,sm,isub,smx,smy,smz,mx,my,mz
common /spins/ sx,sy,sz,nx,ny,nz,beta1,beta2    
end module input_module_3d






program kag_dipole_3d
use input_module_3d
implicit none 

!write(*,*) "L: ",L," Trans: ",ntrans," Meas: ",nmeas," Number of steps: ",nfield1  
write(10,*) "L: ",L," Trans: ",ntrans," Meas: ",nmeas," Number of steps: ",nfield1  
write(10,*) "T: ",T
!call precisiontest
!ns counts number of sites
!nz0 counts sublattice D  (0,0,0)
!nz1 counts sublattice A  (0,1,1)
!nz2 counts sublattice B  (1,1,0)
!nz3 counts sublattice C  (1,0,1)

ns=0
nz0=0
nz1=0
nz2=0
nz3=0
!ix,iyy,iz are integer components along non-orthogonal basis vectors
do ix=1,L
do iyy=1,L
do iz=1,L
Ns=Ns+1
!set all spin lengths to unity
sm(ns)=1
!set spin length to zero on D sites for fcckagome and to unity for fcc
if(((ix+iyy-2*((ix+iyy)/2)).eq.0).and.((iyy+iz-2*((iyy+iz)/2)).eq.0).and.((ix+iz-2*((ix+iz)/2)).eq.0)) then
sm(ns)=0
nz0=nz0+1
isub(ns)=0
endif

!label sublattice given site number  

if(((ix+iyy-2*((ix+iyy)/2)).eq.0).and.((iyy+iz-2*((iyy+iz)/2)).ne.0).and.((ix+iz-2*((ix+iz)/2)).ne.0)) then 
isub(ns)=1
nz1=nz1+1
endif
if(((ix+iyy-2*((ix+iyy)/2)).ne.0).and.((iyy+iz-2*((iyy+iz)/2)).eq.0).and.((ix+iz-2*((ix+iz)/2)).ne.0)) then
isub(ns)=2
nz2=nz2+1
endif
if(((ix+iyy-2*((ix+iyy)/2)).ne.0).and.((iyy+iz-2*((iyy+iz)/2)).ne.0).and.((ix+iz-2*((ix+iz)/2)).eq.0)) then
isub(ns)=3
nz3=nz3+1
endif
!record site number for each space vector
table(ix,iyy,iz)=ns
!create inverse table
itable(ns,1)=ix
itable(ns,2)=iyy
itable(ns,3)=iz
enddo
enddo
enddo
!determine spin neighbours and boundary conditions
Do i=1,nsite
   If (sm(i)==0) Go To 53
   ix=itable(i,1)
   iyy=itable(i,2)
   iz=itable(i,3)

   ixp=ix+1
   ixm=ix-1
   iyyp=iyy+1
   iyym=iyy-1
   izp=iz+1
   izm=iz-1
   
   If (ix==L) ixp=1
   If (ix==1) ixm=L
   If (iyy==L) iyyp=1
   If (iyy==1) iyym=L
   If (iz==L) izp=1
   If (iz==1) izm=L

   nbr(i,1)=table(ixp,iyy,iz)
   nbr(i,2)=table(ix,iyyp,iz)
   nbr(i,3)=table(ix,iyy,izp)
   nbr(i,4)=table(ixm,iyy,iz)
   nbr(i,5)=table(ix,iyym,iz)
   nbr(i,6)=table(ix,iyy,izm)
   nbr(i,7)=table(ixm,iyyp,iz)
   nbr(i,8)=table(ixm,iyy,izp)
   nbr(i,9)=table(ixp,iyym,iz)
   nbr(i,10)=table(ixp,iyy,izm)
   nbr(i,11)=table(ix,iyyp,izm)
   nbr(i,12)=table(ix,iyym,izp)
   53 Continue
Enddo
Do j=1,3
Do i=1,nsite
   If (isub(i).eq.j) Write(2,*) itable(i,1),itable(i,2),itable(i,3)
EndDo
EndDo
!set all spin components to zero
sx=0
sy=0
sz=0
nocc=0
do i=1,nsite
nocc=nocc+sm(i)
enddo
!construct the dipole matrix for the given choice of basis vectors(in the subroutine)
!remove if not calculating dipole interactions

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! If you have generated the dipole matrices for a given size
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  and written them to a file, it saves time to read them in
!do ial=1,3
!do ibet=ial,3
!do kk=0,L-1
!do jj=0,L-1
!do ii=0,L-1
!read(30+10*ial+ibet,*) i,j,k,D(ial,ibet,ii,jj,kk)
!D(ibet,ial,ii,jj,kk)=D(ial,ibet,ii,jj,kk)
!enddo
!enddo
!enddo
!enddo
!enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
jex=0
di=1
Hmag=0
Htheta=0.785398
Hphi=0.785398

!call setField

!Random initialization
!Call init_random_seed()
!Call random_number(rnd)
!ran=INT(rnd*100)
!Do i=1,ran
!call spinit
!EndDo
!Preset intialization
call spinitPreset
!if (degauss.eq.1) then
!fieldsteps=2*nfield1
!else
!fieldsteps=nfield1
!end if
!write(*,*) "Entering loop"

do i2=0,etasteps
eta0=etamin+(i2)*(etamax-etamin)/(etasteps)
call gamma(L,D,eta0)




!Set averages to zero 
sm2av=0.
entav=0.
mcav2=0.
mcav=0.
msav2=0.
msav=0.
Eav=0.
Eav2=0.
Eav4=0.
mav=0.
mav2=0.
mzav=0.
myav=0.
mxav=0.
rmav2=0.0
rmav=0.0
rmxav=0.0
rmyav=0.0
rmzav=0.0
rmav4=0.0
cmxav=0.
cmyav=0.
cmzav=0.
cm2av=0.
rm2root=0.
!if(degauss.eq.1)then
!	if(i2.le.nfield1)then
!		Hmag=Hmin1+I2*(Hmax1-Hmin1)/(nfield1)
!	else
!		Hmag=Hmax1-(I2-nfield1)*(Hmax1-Hmin1)/(nfield1)
!	end if
!else
!	Hmag=Hmin1+I2*(Hmax1-Hmin1)/(nfield1)
	!Hmag=Hmax1-I2*(Hmax1-Hmin1)/(nfield1)
!end if

!hmx=Hmag*dsin(Htheta)*dcos(Hphi)
!hmy=Hmag*dsin(Htheta)*dsin(Hphi)
!hmz=Hmag*dcos(Htheta)
call measure
!write(*,*) hmag,E/nocc
!garbtmp=ntrans/intervals
!duration=floor(garbtmp)
!counter=0
!Ecurr=E/nocc
!Eprev=0
!TOL=1
!Not running EFM. Just measuring.
!DO WHILE (COUNTER .LE. INTERVALS .AND. TOL .GT. CON) 
!	DO IMEAS=1,DURATION
!		CALL EFM
!	ENDDO
!	write(*,*) 'counter',counter
!	CALL MEASURE
!	Eprev=Ecurr
!	Ecurr=E/nocc
!	tol=dabs(Ecurr-Eprev)/dabs(Ecurr)
!	COUNTER=COUNTER+1
!ENDDO

do i=0,3
sm2av(i)=sm2av(i)+smx(i)*smx(i)+smy(i)*smy(i)+smz(i)*smz(i)
enddo
 mzav=mzav+mz
 mxav=mxav+mx
 myav=myav+my
 mav2=mav2+mx*mx+my*my+mz*mz
 EAV=Eav+e
 EAV2=EAV2+(E*E)
 Eav4=eav4+(E*E*E*E)

rmav2=rmav2+rmx*rmx+rmy*rmy+rmz*rmz
rmav4=rmav4+(rmx*rmx+rmy*rmy+rmz*rmz)**2
rmxav=rmxav+(rmx)
rmyav=rmyav+(rmy)
rmzav=rmzav+(rmz)
rm2root=rm2root+dsqrt(rmx*rmx+rmy*rmy+rmz*rmz)

sm2av=sm2av/nmeas
smav=dsqrt(sm2av)/nz1
entav=entav/nmeas
entav=-log(entav)/nsite
rmav2=rmav2/nmeas
rmav4=rmav4/nmeas
rmxav=rmxav/nmeas
rmyav=rmyav/nmeas
rmzav=rmzav/nmeas
rm2root=rm2root/nmeas
rmav=dsqrt(rmav2)/(nocc)

chi=(rmav2-rm2root**2)/T
chi=chi/nocc

ub=1-rmav4/(3*rmav2*rmav2)
ub=rmav4/(rmav2*rmav2)


mzav=mzav/nmeas
myav=myav/nmeas
mxav=mxav/nmeas
 mav2=mav2/nmeas
 eav2=eav2/nmeas
 eav4=eav4/nmeas
 mcav2=mcav2/nmeas
 msav2=msav2/nmeas
 eav=eav/nmeas
 mzav=mzav/(nocc)
mxav=mxav/(nocc)
myav=myav/(nocc)
UE=1-Eav4/(3*Eav2*eav2)
 mav=sqrt(mav2)/(nocc)
 CVT=(EAV2-EAV*EAV)/(T*T)/(nocc)


write(*,*) eta0,E/nocc
write(1,999) eta0,EAV/nocc
!Write spin components to a file. Place at the end of the do loop after EFM sweeps
Write(conffile,"('conf',I4.4,'.dat')") i2 !changes the name of file for each iteration of loop, e.g. conf0001.dat. Change the variable i2 to your loop counter.
Open(Unit=98,FILE=conffile,STATUS='REPLACE')
Do i4=1,3
Do i5=1,nsite
If (isub(i5).eq.i4) Then
Write(98,*) sx(i5),sy(i5),sz(i5)
End If
EndDo
EndDo

enddo 

999 format(1x,8e16.6)
end program kag_dipole_3d
!!!!!!!!!!!!!!!!!!!!!

subroutine spinitPreset
use input_module_3d
implicit none
integer :: i10
!initialize spins according to spins read from file
open (unit=99, file='spin0000.dat', status='old', action='read')
	do i11=1,3
	Do i12=1,nsite
	if (isub(i12).eq.i11) Then
		read(99,*) sx(i12),sy(i12),sz(i12)
	end if
	EndDo
	enddo
close(99)
write(10,*) "Preset Spin Configuration"
end subroutine spinitPreset

subroutine setField
use input_module_3d
!Read in the theta and phi angles for the magnetic field vector
open (unit=44, file='fieldAngles.dat', status='old', action='read')
read(44,*) Hphi,Htheta
close(44)
end subroutine setField

!!!!!!!!!!!!!!!!!!!!!
subroutine spinit
use input_module_3d
implicit none
! initialize all spins in the particle in random directions

            DO IT=1,nsite
               
                   
            CT=-1.+2.*r250_(iseed)
             st=dsqrt(1.-ct*ct)
             PHI=2*pi*r250_(iseed)
sx(it)=sm(it)*st*dcos(phi)
sy(it)=sm(it)*st*dsin(phi)
sz(it)=sm(it)*ct
end do
write(10,*) "Random Initial configuration"
end subroutine spinit
!!!!!!!!!!!!!!!!!!!!!!!!!!
  double precision function r250_(idum)
      implicit double precision (a-h,o-z)
      integer idum,im1,im2,imm1,ia1,ia2,iq1,iq2,ntab,ndiv
 real*8 ran2_new,am,eps,rnmx
      parameter (im1=2147483563,im2=2147483399,am=1.d0/im1,imm1=im1-1,&
     &    ia1=40014,ia2=40692,iq1=53668,iq2=52774,ir1=12211, &
     &     ir2=3791,ntab=32,ndiv=1+imm1/ntab,eps=1.2d-14,rnmx=1.d0-eps)
!     
      integer idum2,j,k,iv(ntab),iy
      save    iv,iy,idum2
      data idum2/123456789/,iv/ntab*0/,iy/0/
      
      if(idum.le.0) then
         idum=max(-idum,1)
         idum2=idum
         do j=ntab+8,1,-1
            k=idum/iq1
            idum=ia1*(idum-k*iq1)-k*ir1
            if(idum.lt.0) idum=idum+im1
            if(j.le.ntab) iv(j) = idum
         end do
         iy = iv(1)
      end if
!     
      k=idum/iq1
      idum=ia1*(idum-k*iq1)-k*ir1
                if(idum.lt.0) idum=idum+im1
!     
      k=idum2/iq2
      idum2=ia2*(idum2-k*iq2)-k*ir2
      if(idum2.lt.0) idum2=idum2+im2
      j = 1+iy/ndiv
      iy = iv(j)-idum2
      iv(j) = idum
      if(iy.lt.1) iy=iy+imm1 
      r250_ = min(am*iy,rnmx)
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine precisiontest
use input_module_3d
implicit none

double precision :: x56,inc56,prec56,pi56,precpi56
precpi56=3.14159265358979323846264338327950288419716939937510
inc56=0.01
prec56=0.000000000000001!precision limit
x56=0
do while (dabs((dsin(x56)-dcos(x56)))>prec56)

	do while (dsin(x56) > dcos(x56))
		x56=x56-inc56
	end do
	
	do while (dcos(x56) > dsin(x56))
		x56=x56+inc56
	end do
	
	inc56=inc56/10
end do
pi56=4*x56
write(*,*) pi56

return
end subroutine precisiontest
!!!!!!!!!!!!!!!!!!!!
subroutine sweep
use input_module_3d
implicit none

Do it=1,nsite
if(sm(it).eq.0) go to 55
hpx1=0.
 hpy1=0.
 hpz1=0.
do j=1,nsite
ix=(itable(j,1)-itable(it,1))
iyy=(itable(j,2)-itable(it,2))
iz=(itable(j,3)-itable(it,3))
if(ix<0) then 
ix=ix+L 
endif
if(iyy<0) then 
iyy=iyy+L 
endif
if(iz<0) then 
iz=iz+L 
endif
s(j,1)=sx(j)
s(j,2)=sy(j)
s(j,3)=sz(j)

do k=1,3
hpx1=hpx1-D(1,k,ix,iyy,iz)*s(j,k)
hpy1=hpy1-D(2,k,ix,iyy,iz)*s(j,k)
hpz1=hpz1-D(3,k,ix,iyy,iz)*s(j,k)
enddo 


 enddo



 
 hpx1=hpx1/t
 hpy1=hpy1/t
 hpz1=hpz1/t

 hmp=hpx1**2+hpy1**2+hpz1**2
 if(hmp.lt.1.d-06) go to 55
 hm=dsqrt(hmp)
 hmi=1./hm

 hx=hpx1*hmi
 hy=hpy1*hmi
 hz=hpz1*hmi
 sth=dsqrt(hx**2+hy**2)
 sthi=1./sth
 cph=hx*sthi
 sph=hy*sthi

 phi=2*pi*r250_(iseed)
 cp=dcos(phi)
 sp=dsin(phi)
 y=r250_(iseed)
 ct=1.0+dlog((1.0-y)*dexp(-2.0*hm)+y)*hmi
 IF (ct.gt.1)ct=1.
 st=dsqrt(dabs(1.0-ct*ct))  
 sx(it)=(hz*cph*st*cp-sph*st*sp+hx*ct)
 sy(it)=(hz*sph*st*cp+cph*st*sp+hy*ct)
 sz(it)=(-sth*st*cp+hz*ct)
 55 continue
 
  end do
  return
  end subroutine sweep
  !!!!!!!!!!!!!!!!!!!!!!!
  subroutine measure
use input_module_3d
 implicit none
  E=0
  mx=0.
  my=0.
  mz=0.
  mcx=0.
  mcy=0.
  mcz=0.
  msx=0.
  msy=0.
  msz=0.
  smx=0.
smy=0.
smz=0.
smxalt=0.
smyalt=0.
smzalt=0.
rmx=0.0
rmy=0.0
rmz=0.0
  Do IT=1,Nsite
  do i=0,3
  if(isub(it).eq.i) then
  smx(i)=smx(i)+sx(it)*(-1)**(itable(it,1)+itable(it,2)+itable(it,3))
  smy(i)=smy(i)+sy(it)*(-1)**(itable(it,1)+itable(it,2)+itable(it,3))
  smz(i)=smz(i)+sz(it)*(-1)**(itable(it,1)+itable(it,2)+itable(it,3))
  smxalt(i)=smxalt(i)+sx(it)
  smyalt(i)=smyalt(i)+sy(it)
  smzalt(i)=smzalt(i)+sz(it)
  endif
  enddo
  rmx=rmx+sx(it)
  rmy=rmy+sy(it)
  rmz=rmz+sz(it)

  mx=mx+sx(it)
  my=my+sy(it)
  mz=mz+sz(it)

hpx1=0.0
hpy1=0.0
hpz1=0.0

Do j=1,nsite
   ix=(itable(j,1)-itable(it,1))
   iyy=(itable(j,2)-itable(it,2))
   iz=(itable(j,3)-itable(it,3))
   If (ix<0) ix=ix+L
   If (iyy<0) iyy=iyy+L
   If (iz<0) iz=iz+L
   s(j,1)=sx(j)
   s(j,2)=sy(j)
   s(j,3)=sz(j)

   Do k=1,3
      hpx1=hpx1-D(1,k,ix,iyy,iz)*s(j,k)
      hpy1=hpy1-D(2,k,ix,iyy,iz)*s(j,k)
      hpz1=hpz1-D(3,k,ix,iyy,iz)*s(j,k)
   Enddo

Enddo

 E=E-di*(hpx1*sx(it)+hpy1*sy(it)+hpz1*sz(it))-2*(hmx*sx(it)+hmy*sy(it)+hmz*sz(it))
 enddo
E=E/2

 ! call exchange
 ! E=E+hamil
  return
end subroutine measure

!!!!!!!!!!!!
  subroutine metro
  use input_module_3d
	implicit none
double precision :: eold,enew,sx1,sy1,sz1,sdot1,de

Do is=1,Nsite
if(sm(is).eq.0) go to 55
it=is

nbrx=0.0
nbry=0.0
nbrz=0.0
 
Do j=1,12
   nbrx(it)=nbrx(it)+sx(nbr(it,j))
   nbry(it)=nbry(it)+sy(nbr(it,j))
   nbrz(it)=nbrz(it)+sz(nbr(it,j))
Enddo
  
hpx1=0.0
hpy1=0.0
hpz1=0.0

Do j=1,nsite
   ix=(itable(j,1)-itable(it,1))
   iyy=(itable(j,2)-itable(it,2))
   iz=(itable(j,3)-itable(it,3))
   If (ix<0) ix=ix+L
   If (iyy<0) iyy=iyy+L
   If (iz<0) iz=iz+L
   s(j,1)=sx(j)
   s(j,2)=sy(j)
   s(j,3)=sz(j)

   Do k=1,3
      hpx1=hpx1-D(1,k,ix,iyy,iz)*s(j,k)
      hpy1=hpy1-D(2,k,ix,iyy,iz)*s(j,k)
      hpz1=hpz1-D(3,k,ix,iyy,iz)*s(j,k)
   Enddo

Enddo

 eold=jex*(sx(it)*nbrx(it)+sy(it)*nbry(it)+sz(it)*nbrz(it))-di*(hpx1*sx(it)+hpy1*sy(it)+hpz1*sz(it))
 sxtmp=sx(it)
 sytmp=sy(it)
 sztmp=sz(it)
 
 phi=2*pi*r250_(iseed)
 cp=dcos(phi)
 sp=dsin(phi)
 ct=-1.+2*r250_(iseed)
 st=dsqrt(dabs(1.0-ct*ct))
 sx1=st*cp
 sy1=st*sp
 sz1=ct

stnew=dsqrt(sx1*sx1+sy1*sy1+sz1*sz1)

sx1=sx1/stnew
sy1=sy1/stnew
sz1=sz1/stnew

sx(it)=sx1
sy(it)=sy1
sz(it)=sz1

enew=jex*(sx(it)*nbrx(it)+sy(it)*nbry(it)+sz(it)*nbrz(it))-di*(hpx1*sx(it)+hpy1*sy(it)+hpz1*sz(it))

DE=Enew-Eold
                 y=r250_(iseed)
                 If(y.ge.(dexp(-de/t)/(1.+dexp(-de/t)))) then
                 sx(it)=sxtmp
                 sy(it)=sytmp
                 sz(it)=sztmp
                 endif 
 
  55 continue
  end do
  return
  end subroutine metro

subroutine efm
use input_module_3d
implicit none

Do is=1,nsite
if (sm(is)==0) go to 56
it=is

nbrx=0.0
nbry=0.0
nbrz=0.0

Do j=1,12
   nbrx(it)=nbrx(it)+sx(nbr(it,j))
   nbry(it)=nbry(it)+sy(nbr(it,j))
   nbrz(it)=nbrz(it)+sz(nbr(it,j))
EndDo

hpx1=0.0
hpy1=0.0
hpz1=0.0

Do j=1,nsite
   ix=(itable(j,1)-itable(it,1))
   iyy=(itable(j,2)-itable(it,2))
   iz=(itable(j,3)-itable(it,3))
   If (ix<0) ix=ix+L
   If (iyy<0) iyy=iyy+L
   If (iz<0) iz=iz+L
   s(j,1)=sx(j)
   s(j,2)=sy(j)
   s(j,3)=sz(j)

   Do k=1,3
      hpx1=hpx1-D(1,k,ix,iyy,iz)*s(j,k)
      hpy1=hpy1-D(2,k,ix,iyy,iz)*s(j,k)
      hpz1=hpz1-D(3,k,ix,iyy,iz)*s(j,k)
   EndDo
EndDo

sxtmp=di*hpx1-(jex*nbrx(it))+hmx
sytmp=di*hpy1-(jex*nbry(it))+hmy
sztmp=di*hpz1-(jex*nbrz(it))+hmz

stnew=dsqrt(sxtmp*sxtmp+sytmp*sytmp+sztmp*sztmp)

sx(it)=sxtmp/stnew
sy(it)=sytmp/stnew
sz(it)=sztmp/stnew

56 Continue
Enddo
end subroutine efm

subroutine randefm
use input_module_3d
implicit none


Do is=1,nsite
!write(*,*) "Within randefm"

it=NINT(nsite*r250_(iseed))
!write(*,*) it, NINT(nsite*r250_(iseed)), r250_(iseed), iseed
!write(*,*) bit_size(it), bit_size(NINT(nsite*r250_(iseed))), precision(r250_(iseed)), bit_size(iseed)
!write(*,*) "Entering if statement",is,bit_size(is)
!it=is
if(it==0) go to 56
!write(69,*) it
if (sm(it)==0) go to 56
nbrx=0.0
nbry=0.0
nbrz=0.0
!write(*,*) "Debug 1"
Do j=1,12
   nbrx(it)=nbrx(it)+sx(nbr(it,j))
   nbry(it)=nbry(it)+sy(nbr(it,j))
   nbrz(it)=nbrz(it)+sz(nbr(it,j))
EndDo
!write(*,*) "Debug 2"
hpx1=0.0
hpy1=0.0
hpz1=0.0

Do j=1,nsite
   ix=(itable(j,1)-itable(it,1))
   iyy=(itable(j,2)-itable(it,2))
   iz=(itable(j,3)-itable(it,3))
   If (ix<0) ix=ix+L
   If (iyy<0) iyy=iyy+L
   If (iz<0) iz=iz+L
   s(j,1)=sx(j)
   s(j,2)=sy(j)
   s(j,3)=sz(j)

 Do k=1,3
      hpx1=hpx1-D(1,k,ix,iyy,iz)*s(j,k)
      hpy1=hpy1-D(2,k,ix,iyy,iz)*s(j,k)
      hpz1=hpz1-D(3,k,ix,iyy,iz)*s(j,k)
   EndDo
EndDo
!write(*,*) "Debug 3"

sxtmp=di*hpx1-(jex*nbrx(it))+hmx
sytmp=di*hpy1-(jex*nbry(it))+hmy
sztmp=di*hpz1-(jex*nbrz(it))+hmz
!write(*,*) "Debug 4"
stnew=dsqrt(sxtmp*sxtmp+sytmp*sytmp+sztmp*sztmp)

sx(it)=sxtmp/stnew
sy(it)=sytmp/stnew
sz(it)=sztmp/stnew
!write(*,*) "Debug 5"

56 Continue
!write(*,*) "Debug 6"

Enddo
!write(*,*) "Debug 7"
end subroutine randefm


Subroutine Gamma(L,D,eta0)

implicit none
!use numerical_libraries
double precision :: a(3),b(3),c(3),u1(3),u2(3),u3(3),u2xu3(3),u3xu1(3),u1xu2(3),b1(3),b2(3),b3(3),vu
double precision :: b1xb2(3)
double precision :: u1xu2dotu3,norm_u1xu2,b1xb2dotb3,norm_b1xb2
double precision :: Q(3),r(3),R1(3),Q2,Q1,rdotQ,rRab,rR2,rR1,rR4,rR5,F,G,D(3,3,0:L-1,0:L-1,0:L-1)
double precision, parameter :: pi=3.14159265358979323846264338327
double precision, parameter :: sqrt_pi=1.77245385090551602729816748334
double precision, parameter :: tau=1.d-3
double precision :: eta0,eta,eta2
integer :: R3(3),L,i,ibet,ii,jj,kk,nsize=15,n,k,m,ial
double precision :: rcut,kcut,boxHeight,k_boxHeight
!Double precision parameters originally only went to like 17 decimal places. Maybe that's 
!limiting precision to only 17 decimal places. 
eta=eta0
eta2=eta*eta
!Why are there two different assignments of basis vectors?
!a is a1
a(1)=1
a(2)=0
a(3)=0
!b is a2
b(1)=0.5d0
b(2)=dsqrt(3.d0)/2
b(3)=0
!c is a3
!Together, these make up the real space basis vectors 
c(1)=0.5d0
c(2)=dsqrt(3.d0)/6
c(3)=dsqrt(2.d0)/dsqrt(3.d0)

!ogg gog and ggo
a(1)=0
a(2)=1/dsqrt(2.d0)
a(3)=1/dsqrt(2.d0)
b(1)=1/dsqrt(2.d0)
b(2)=0
b(3)=1/dsqrt(2.d0)
c(1)=1/dsqrt(2.d0)
c(2)=1/dsqrt(2.d0)
c(3)=0
!Together, THESE actually make up the real space basis vectors 

do i=1,3
u1(i)=L*a(i)
u2(i)=L*b(i)
u3(i)=L*c(i)
!I thought u3 was a cross product of a and b, normalized by the magnitude of that cross product
enddo
!Cross product calculations
u2xu3(1)=u2(2)*u3(3)-u2(3)*u3(2)
u2xu3(2)=u2(3)*u3(1)-u2(1)*u3(3)
u2xu3(3)=u2(1)*u3(2)-u2(2)*u3(1)
vu=0
do i=1,3
vu=vu+u1(i)*u2xu3(i)!Volume of the unit cell 
enddo
vu=dabs(vu)
u3xu1(1)=u3(2)*u1(3)-u3(3)*u1(2)
u3xu1(2)=u3(3)*u1(1)-u3(1)*u1(3)
u3xu1(3)=u3(1)*u1(2)-u3(2)*u1(1)
u1xu2(1)=u1(2)*u2(3)-u1(3)*u2(2)
u1xu2(2)=u1(3)*u2(1)-u1(1)*u2(3)
u1xu2(3)=u1(1)*u2(2)-u1(2)*u2(1)
do i=1,3
b1(i)=2*pi*u2xu3(i)/vu!Reciprocal lattice basis vectors 
b2(i)=2*pi*u3xu1(i)/vu
b3(i)=2*pi*u1xu2(i)/vu
enddo

!Calculate the height of the central unit in real space.
!Use it in the real space radial cutoff
u1xu2dotu3=0
norm_u1xu2=0
do i=1,3
u1xu2dotu3=u1xu2dotu3+u1xu2(i)*u3(i)!u1xu2 dotted with u3
norm_u1xu2=norm_u1xu2 + u1xu2(i)*u1xu2(i)!Squared norm of u1xu2
enddo
boxHeight=u1xu2dotu3/norm_u1xu2
write(*,*) 'boxHeight: ',boxHeight
!Calculate the real space radial cutoff
rcut=(nsize-1)*boxHeight


b1xb2(1)=b1(2)*b2(3)-b1(3)*b2(2)
b1xb2(2)=b1(3)*b2(1)-b1(1)*b2(3)
b1xb2(3)=b1(1)*b2(2)-b1(2)*b2(1)
!Calculate the height of the central unit in reciprocal space.
!Use it in the reciprocal space radial cutoff
b1xb2dotb3=0
norm_b1xb2=0
do i=1,3
b1xb2dotb3=b1xb2dotb3+b1xb2(i)*b3(i)!u1xu2 dotted with u3
norm_b1xb2=norm_b1xb2 + b1xb2(i)*b1xb2(i)!Squared norm of u1xu2
enddo
k_boxHeight=b1xb2dotb3/norm_b1xb2
write(*,*) 'k_boxHeight: ',k_boxHeight
!Calculate the real space radial cutoff
kcut=(nsize-1)*k_boxHeight



do ial=1,3
do ibet=ial,3


!r3(3)are just integer coefficients
do kk=0,L-1
r3(3)=kk
do jj=0,L-1
r3(2)=jj
do ii=0,L-1
r3(1)=ii
F=0
G=0
!Why is this starting at zero and ending at L-1? Assuming zero is basically L=12 because of periodic

do i=1,3
r(i)=r3(1)*a(i)+r3(2)*b(i)+r3(3)*c(i)
enddo


do n=-nsize,nsize
do m=-nsize,nsize
do k=-nsize,nsize
do i=1,3
Q(i)=n*b1(i)+m*b2(i)+k*b3(i)!Arbitrary reciprocal lattice vector 
R1(i)=n*u1(i)+m*u2(i)+k*u3(i)+r(i)!Super lattice vector. Spans entirety of the lattice
enddo
Q2=Q(1)*Q(1)+Q(2)*Q(2)+Q(3)*Q(3)
Q1=dsqrt(Q2)!Length of reciprocal lattice vector 
rdotQ=r(1)*Q(1)+r(2)*Q(2)+r(3)*Q(3)
!if (Q1.lt.1.d-05 ) go to 50!rhombozoidal cutoff
if (Q1.lt.1.d-05 .OR. Q1.gt.kcut) go to 50!spherical cutoff

if(kk.eq.0.and.jj.eq.0.and.ii.eq.0) then
!F is the fourier coefficients 
F=F-4*pi*dexp(-Q2/(4*eta2))*(Q2*ndelta(ial,ibet)-3*Q(ial)*Q(ibet))/(3*vu*Q2)
!Why subtract from the coefficients for this one particular case? 
!Seems we can only ever calculate gamma except when |rij|=0. Why?
else
F=F+4*pi*Q(ial)*Q(ibet)*dcos(rdotQ)*dexp(-Q2/(4*eta2))/(Q2*vu)
!Nothing wrong with the equation...
endif
50 continue 
!Different values obtained from R, the super lattice vector  
rRab=R1(ial)*R1(ibet)
rR2=R1(1)*R1(1)+R1(2)*R1(2)+R1(3)*R1(3)
rR1=dsqrt(rR2)
rR4=rR2*rR2
rR5=rR4*rR1
!if(rR1.lt.1.d-5 ) go to 51!Rhombozoidal cutoff
if(rR1.lt.1.d-5 .OR. rR1 .GT. rcut ) go to 51!Spherical cutoff

if(kk.eq.0.and.jj.eq.0.and.ii.eq.0) then
G=G+(rR2*ndelta(ial,ibet)-3*R1(ial)*R1(ibet))*(2*eta*dexp(-rR2*eta2)*(3+2*rR2*eta2)/(3*rR4*sqrt_pi)+derfc(rR1*eta)/rR5)
!!!!The first term that is not G is for when R!=0, not when it is.!!! 
else
!Not sure where this one comes from. 
G=G+2*eta*dexp(-eta2*rR2)*(rR2*ndelta(ial,ibet)-3*rRab-2*eta2*rRab*rR2)/(sqrt_pi*rR4)+(rR2*ndelta(ial,ibet)&
&-3*rRab)*derfc(eta*rR1)/rR5
endif
51 continue
enddo
enddo
enddo
69 continue
write(30+10*ial+ibet,*) r3,F+G
D(ial,ibet,ii,jj,kk)=F+G
D(ibet,ial,ii,jj,kk)=F+G
enddo
enddo
enddo

enddo
enddo
contains 
	integer function ndelta(i,j)
	integer :: i,j
!Kronecker Delta function
	ndelta=0
	if(i.eq.j) ndelta=1
end function ndelta
end subroutine Gamma

Subroutine exchange
   use input_module_3d
implicit none
hamil=0.0
nbrx=0.0
nbry=0.0
nbrz=0.0

Do i=1,nsite
   If (sm(i)==0) Go To 52
   Do j=1,12
      nbrx(i)=nbrx(i)+sx(nbr(i,j))
      nbry(i)=nbry(i)+sy(nbr(i,j))
      nbrz(i)=nbrz(i)+sz(nbr(i,j))
   Enddo

   hamil=hamil+jex*(sx(i)*nbrx(i)+sy(i)*nbry(i)+sz(i)*nbrz(i))
   52 Continue
Enddo
   hamil=hamil/2

End Subroutine

Subroutine init_random_seed()
implicit none
!Create random seed
         Integer :: i, n, clock
         Integer, Dimension(:), Allocatable :: seed

         Call random_seed(size = n)
         Allocate(seed(n))

         Call system_clock(Count=clock)

         seed = clock + 37 * (/ (i - 1, i = 1, n) /)
         Call random_seed(PUT = seed)

         Deallocate(seed)
End Subroutine
