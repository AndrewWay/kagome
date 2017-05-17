program gamma

implicit none
!use numerical_libraries
double precision :: a(3),b(3),c(3),u1(3),u2(3),u3(3),u2xu3(3),u3xu1(3),u1xu2(3),b1(3),b2(3),b3(3),vu
double precision :: Q(3),r(3),R1(3),Q2,Q1,rdotQ,rRab,rR2,rR1,rR4,rR5,F,G,D(3,3,0:L-1,0:L-1,0:L-1)
double precision, parameter :: pi=3.14159265358979323846264338327
double precision, parameter :: sqrt_pi=1.77245385090551602729816748334
double precision, parameter :: tau=1.d-3,eta=1.2d0,eta2=eta*eta
integer :: R3(3),L,i,ibet,ii,jj,kk,nsize=15,n,k,m,ial
!Double precision parameters originally only went to like 17 decimal places. Maybe that's 
!limiting precision to only 17 decimal places. 

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
if (Q1.lt.1.d-05) go to 50!Something weird happens at Q1=0. So we choose a number close to it?
!0.00001 isn't super close to 0. What happens if make the exponent more negative? Better precision?
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
if(rR1.lt.1.d-5) go to 51!Again, the less than 0.00005. Are these for checking convergence?
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
end program gamma
