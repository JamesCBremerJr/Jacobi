!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  Copyright 2017 by James Bremer and Haizhao Yang
!
!  This program is free software: you can redistribute it and/or modify it under the terms 
!  of the GNU General Public License as published by the Free Software Foundation, either 
!  version 3 of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
!  See the GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License along with
!  this program.  If not, see <http://www.gnu.org/licenses/>.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  This file contains code for constructing Gauss-Jacobi quadrature rules.  The n-point
!  Gauss-Jacobi rule associated with the parameters (da,db) is the quadrature rule
!
!         1                           n
!    \int (1-x)^da (1+x)^db p(x) ~  \sum p(x_j) w_j                                       (1)
!        -1                          j=1
! 
!  which is exact when p is a polynomial of degree 2n-1.  The n-point modified
!  Gauss-Jacoi quadrature rule is 
!
!        \pi                          
!    \int    cos(t/2)^(2db+1) sin(t/2)^(2da+1) p(cos(t)) dt ~ 
!         0                                                                               (2)
!
!                    n
!                  \sum sin(t/2)^(2db+1) cos(t/2)^(2da+1) p(cos(t_j)) w_j .
!                   j=1
!
!  It is exact when p is a polynomial of degree less than or equal to 2n-1.  Note that
!  the importance of the modified rule is that it integrates products of the functions
!  obtained by introducing the change of variables x = cos(t) into Jacobi's differential
!  equation.
!
!  The following subroutines should be regarded as publicly callable:
!
!    jacobi_quad -  construct the quadrature rule (1)
!
!    jacobi_quad_mod - construct the quadrature rule (2)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



subroutine jacobi_quad(n,da,db,xs,whts)
use chebyshev
implicit double precision (a-h,o-z)
integer, intent(in)           :: n
double precision, intent(in)  :: da,db
double precision, intent(out) :: xs(n),whts(n)

!
!  Return the n-point Gauss-Jacobi rule (1).
!
!  Input parameters:
!    n - the length of the desired quadrature rule
!    (da,db) - the parameters for Jacobi's differential equation
!
!  Output parameters:
!    xs - this user-supplied vector of length n will contain the nodes of the 
!      Gauss-Jacobi quadrature rule
!    whts - this user-supplied vector of length n will contain the quadrature
!      weights
!

double precision, allocatable :: ab(:,:),avals(:,:),psivals(:,:),whts0(:)
double precision, allocatable :: abinv(:,:),alphainv(:,:),alphainvp(:,:)


double precision, allocatable :: avals2(:,:),psivals2(:,:)
double precision, allocatable :: abinv2(:,:),alphainv2(:,:),alphainvp2(:,:)

type(chebexps_data)           :: chebdata
data  pi        / 3.14159265358979323846264338327950288d0 /

dnu  = n
p    = dnu+(da+db+1)/2
eps0 = epsilon(0.0d0)
k    = 16
allocate(whts0(k))
call chebexps(k,chebdata)


!
!  Construct the phase function
!

dd         = 1.0d0/dnu*2/pi
dd         = min(0.01d0,dd)
dd         = log(dd)/log(2.0d0)
nints      = ceiling(-dd)
nints      = 2*nints

allocate(ab(2,nints))
call jacobi_phase_disc(nints,ab)


allocate(avals(k,nints),psivals(k,nints))
allocate(avals2(k,nints),psivals2(k,nints))

call  jacobi_phase2(chebdata,dnu,da,db,nints,ab,avals,psivals)
avals(:,1:nints/2)   = avals(:,1:nints/2)**2

call  jacobi_phase2(chebdata,dnu,db,da,nints,ab,avals2,psivals2)
avals2(:,1:nints/2)   = avals2(:,1:nints/2)**2



! determine the number of roots in (0,pi/2)
nroots = (psivals(k,nints/2)-psivals(1,1)) / pi



!
! Invert the phase function
!

allocate(abinv(2,nints),alphainv(k,nints),alphainvp(k,nints))

call jacobi_phase_inverse(nints,ab,chebdata%k,chebdata%xs,chebdata%aintl,chebdata%u, &
 psivals,avals,abinv,alphainv,alphainvp)


allocate(abinv2(2,nints),alphainv2(k,nints),alphainvp2(k,nints))

call jacobi_phase_inverse(nints,ab,chebdata%k,chebdata%xs,chebdata%aintl,chebdata%u, &
 psivals2,avals2,abinv2,alphainv2,alphainvp2)


dconst = pi * 2.0d0**(da+db+1.0d0)

int0 = 1
do i=1,nroots
xx  = pi/2 + (i-1)*pi
idx = n-i+1
do int=int0,nints-1
if (xx .lt. abinv(2,int)) exit
end do
a    = abinv(1,int)
b    = abinv(2,int)
int0 = int

!!!!!!!!!!!!!!!!!!
! call chebeval(a,b,k,chebdata%xs,alphainv(:,int),xx,t)
! call chebeval(a,b,k,chebdata%xs,alphainvp(:,int),xx,apval)
!!!!!!!!!!!!!!!!!!
xxx   = (2*xx - (b+a) ) /(b-a)
dsign = 1
do l=1,k
diff     = xxx-chebdata%xs(l)
if (abs(diff) .lt. eps0) then
t     = alphainv(l,int)
apval = alphainvp(l,int)
goto 1000
endif

whts0(l) = dsign/diff
dsign    = -dsign
end do
whts0(1) = whts0(1)/2
whts0(k) = whts0(k)/2

sumw = sum(whts0)
t     = sum(alphainv(:,int)*whts0)/sumw
apval = sum(alphainvp(:,int)*whts0)/sumw
1000 continue
!!!!!!!!!!!!!!!!!!!!!!!


xs(idx)     = t
r           = cos(t/2)**(2*db+1) * sin(t/2)**(2*da+1)
whts(idx)   = dconst*r*apval
end do


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


int0 = 1
do i=1,n-nroots
xx   = pi/2 + (i-1)*pi
idx  = i
do int=int0,nints-1
if (xx .lt. abinv2(2,int)) exit
end do
a    = abinv2(1,int)
b    = abinv2(2,int)
int0 = int


!!!!!!!!!!!!!!!!!!
!call chebeval(a,b,k,chebdata%xs,alphainv2(:,int),xx,t)
!call chebeval(a,b,k,chebdata%xs,alphainvp2(:,int),xx,apval)
!!!!!!!!!!!!!!!!!!!!!!!!!
xxx   = (2*xx - (b+a) ) /(b-a)
dsign = 1
do l=1,k
diff     = xxx-chebdata%xs(l)
if (abs(diff) .lt. eps0) then
t     = alphainv2(l,int)
apval = alphainvp2(l,int)
goto 2000
endif

whts0(l) = dsign/diff
dsign    = -dsign
end do
whts0(1) = whts0(1)/2
whts0(k) = whts0(k)/2

sumw  = sum(whts0)
t     = sum(alphainv2(:,int)*whts0)/sumw
apval = sum(alphainvp2(:,int)*whts0)/sumw
2000 continue

xs(idx)   = pi-t
r         = cos(t/2)**(2*da+1) * sin(t/2)**(2*db+1)
whts(idx) = dconst *r*apval

end do

xs = cos(xs)

end subroutine




subroutine jacobi_quad_mod(n,da,db,xs,whts)
use chebyshev
implicit double precision (a-h,o-z)
integer, intent(in)           :: n
double precision, intent(in)  :: da,db
double precision, intent(out) :: xs(n),whts(n)

!
!  Return the n-point Gauss-Jacobi rule (1).
!
!  Input parameters:
!    n - the length of the desired quadrature rule
!    (da,db) - the parameters for Jacobi's differential equation
!
!  Output parameters:
!    xs - this user-supplied vector of length n will contain the nodes of the 
!      Gauss-Jacobi quadrature rule
!    whts - this user-supplied vector of length n will contain the quadrature
!      weights
!

double precision, allocatable :: ab(:,:),avals(:,:),psivals(:,:),whts0(:)
double precision, allocatable :: abinv(:,:),alphainv(:,:),alphainvp(:,:)


double precision, allocatable :: avals2(:,:),psivals2(:,:)
double precision, allocatable :: abinv2(:,:),alphainv2(:,:),alphainvp2(:,:)

type(chebexps_data)           :: chebdata
data  pi        / 3.14159265358979323846264338327950288d0 /

dnu  = n
p    = dnu+(da+db+1)/2
eps0 = epsilon(0.0d0)
k    = 16
allocate(whts0(k))
call chebexps(k,chebdata)

!
!  Construct the phase function
!

dd         = 1.0d0/dnu*2/pi
dd         = min(0.01d0,dd)
dd         = log(dd)/log(2.0d0)
nints      = ceiling(-dd)
nints      = 2*nints

allocate(ab(2,nints))
call jacobi_phase_disc(nints,ab)



allocate(avals(k,nints),psivals(k,nints))
allocate(avals2(k,nints),psivals2(k,nints))

call  jacobi_phase2(chebdata,dnu,da,db,nints,ab,avals,psivals)
avals   = avals**2

call  jacobi_phase2(chebdata,dnu,db,da,nints,ab,avals2,psivals2)
avals2   = avals2**2

! determine the number of roots in (0,pi/2)
nroots = (psivals(k,nints/2)-psivals(1,1)) / pi


!
! Invert the phase function
!

allocate(abinv(2,nints),alphainv(k,nints),alphainvp(k,nints))

call jacobi_phase_inverse(nints,ab,chebdata%k,chebdata%xs,chebdata%aintl,chebdata%u, &
 psivals,avals,abinv,alphainv,alphainvp)

allocate(abinv2(2,nints),alphainv2(k,nints),alphainvp2(k,nints))

call jacobi_phase_inverse(nints,ab,chebdata%k,chebdata%xs,chebdata%aintl,chebdata%u, &
 psivals2,avals2,abinv2,alphainv2,alphainvp2)


dconst = pi * 2.0d0**(da+db+1.0d0)

int0 = 1
do i=1,nroots
xx  = pi/2 + (i-1)*pi
idx = i
do int=int0,nints/2-1
if (xx .lt. abinv(2,int)) exit
end do
a    = abinv(1,int)
b    = abinv(2,int)
int0 = int

!!!!!!!!!!!!!!!!!!
!call chebeval(a,b,k,chebdata%xs,alphainv(:,int),xx,t)
!call chebeval(a,b,k,chebdata%xs,alphainvp(:,int),xx,apval)
!!!!!!!!!!!!!!!!!!
xxx   = (2*xx - (b+a) ) /(b-a)
dsign = 1
do l=1,k
diff     = xxx-chebdata%xs(l)
if (abs(diff) .lt. eps0) then
t     = alphainv(l,int)
apval = alphainvp(l,int)
goto 1000
endif

whts0(l) = dsign/diff
dsign    = -dsign
end do
whts0(1) = whts0(1)/2
whts0(k) = whts0(k)/2

sumw = sum(whts0)
t     = sum(alphainv(:,int)*whts0)/sumw
apval = sum(alphainvp(:,int)*whts0)/sumw
1000 continue
!!!!!!!!!!!!!!!!!!!!!!!


xs(idx)     = t
r           = cos(t/2)**(2*db+1) * sin(t/2)**(2*da+1)
whts(idx)   = pi*apval
end do


int0 = 1
do i=1,n-nroots
xx   = pi/2 + (i-1)*pi
idx  = n-i+1
do int=int0,nints/2-1
if (xx .lt. abinv2(2,int)) exit
end do
a    = abinv2(1,int)
b    = abinv2(2,int)
int0 = int


!!!!!!!!!!!!!!!!!!
! call chebeval(a,b,k,chebdata%xs,alphainv2(:,int),xx,t)
! call chebeval(a,b,k,chebdata%xs,alphainvp2(:,int),xx,apval)
!!!!!!!!!!!!!!!!!!!!!!!!!
xxx   = (2*xx - (b+a) ) /(b-a)
dsign = 1
do l=1,k
diff     = xxx-chebdata%xs(l)
if (abs(diff) .lt. eps0) then
t     = alphainv2(l,int)
apval = alphainvp2(l,int)
goto 2000
endif

whts0(l) = dsign/diff
dsign    = -dsign
end do
whts0(1) = whts0(1)/2
whts0(k) = whts0(k)/2

sumw  = sum(whts0)
t     = sum(alphainv2(:,int)*whts0)/sumw
apval = sum(alphainvp2(:,int)*whts0)/sumw
2000 continue

xs(idx)   = pi-t
whts(idx) = pi*apval

end do

end subroutine








subroutine jacobi_phase_inverse(nints,ab,k,xscheb,chebintl,ucheb,alpha,alphap,&
    abinv,alphainv,alphainvp)
use chebyshev
implicit double precision (a-h,o-z)

integer, intent(in)                        :: nints,k
double precision, intent(in)               :: xscheb(k),ab(2,nints),chebintl(k,k)
double precision, intent(in)               :: alpha(k,nints),alphap(k,nints),ucheb(k,k)
double precision, intent(out)              :: alphainv(k,nints),alphainvp(k,nints),abinv(2,nints)

!
!  Compute the inverse of the phase function alpha via Newton's method.
!  
!  Input parameters:
!    (nints,ab) - the discretization scheme for representing the phase
!      function alpha
!    k - the number of terms in the piecewise Chebyshev expansions used to
!      represent the solution
!    xscheb - the nodes of the k-point Chebyshev grid on the interval [-1,1]
!    chebintl - the left Chebyshev spectral integration matrix as returned by 
!      chebexps
!    chebintr - the right Chebyshev spectral integration matrix as returned by 
!      chebexps
!    ucheb - the values-to-coefficients matrix returned by chebexps
!
!  Output parameters:
!
!    (nints,abinv) - the discretization scheme used to represent the inverse of
!      alpha
!   alphainv - a (k,nints) array specifying the values of the inverse of
!     alpha at the nodes of the k-point Chebyshev grids on the intervals
!     in the discretization scheme     
!
!   alphainvp - a (k,nints) array specifying the values of the derivative of
!     the inverse of alpha at the nodes of the k-point Chebyshev grids on the 
!     intervals in the discretization scheme        
!
!

nints0   = nints/2

eps0     = epsilon(0.0d0)
maxiters = 12
eps0     = sqrt(eps0)
nextra   = 3

!
!  Form the initial list of intervals for the inverse function.
!

abinv = 0

do int=1,nints0
a = ab(1,int)
b = ab(2,int)

call chebpw_eval(nints0,ab,k,xscheb,alpha,a,a0)
call chebpw_eval(nints0,ab,k,xscheb,alpha,b,b0)

abinv(1,int) = a0
abinv(2,int) = b0

end do

!
!  Use Newton's method to evaluate the inverse at each of the Chebyhev nodes 
!  on the grid defined by abinv; start at the right-hand side since alpha
!  is monotonically increasing.
!

do int = nints0,1,-1

a0 = abinv(1,int)
b0 = abinv(2,int)
a  = ab(1,int)
b  = ab(2,int)


do i = k,1,-1

x  = (b0-a0)/2*xscheb(i) + (b0+a0)/2
t  = (b-a)/2*xscheb(i) + (b+a)/2

do iter=1,maxiters+1

if (iter .eq. maxiters+1) then
call prina("in jacobi_phase_invert: maximum number of Newton iterations exceeded")
stop
endif

call chebpw_eval(nints0,ab,k,xscheb,alpha,t,val)
call chebpw_eval(nints0,ab,k,xscheb,alphap,t,der)

delta = (val-x)*der
if (abs(delta) .lt. eps0*(1+abs(t))) exit
t     = t - delta

end do


do iextra=1,nextra
call chebpw_eval(nints0,ab,k,xscheb,alpha,t,val)
call chebpw_eval(nints0,ab,k,xscheb,alphap,t,der)
delta = (val-x)*der
t     = t - delta
end do

alphainv(i,int)  = t
alphainvp(i,int) = der

end do
end do

end subroutine
