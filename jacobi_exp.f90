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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  This module contains code for constructing expansions of the phase and amplitude
!  functions for Jacobi's differential equation which vary with degree as well as
!  the argument.  
!
!  The following subroutines are publicly callable:
!
!    jacobi_expansion - given the parameters (da,db), construct an expansion
!      which allows for the O(1) evaluation of Jacobi functions on
!      the interval (0,\pi)
!   
!    jacobi_expansion_eval - given the data generated by jacobi_expansion,
!      evaluate the amplitude and phase at a collection of user-specified points
!
!    jacobi_expansion_size - return the size (in megabytes) of the data stored
!       in an jacobi_expansion_data structure
!
!    jacobi_expansion_test - perform a thorough test of an expansion
!
!    jacobi_expansion_write - write the data describing an expansion to a text file
!      on the disk
!
!    jacobi_expansion_read - read the data describing an expansion from a text file
!      on the disk
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module jacobi_exp

use utils
use chebyshev
use idecomp

type     jacobi_expansion_data
type(chebexps_data)              :: chebdata1,chebdata2
integer                          :: k1,k2,nintsab,nintscd,n,m
double precision                 :: da,db,dmax
double precision, allocatable    :: ab(:,:),cd(:,:)
double precision, allocatable    :: psivals(:,:),avals(:,:)
double precision, allocatable    :: ts(:),dnus(:)         

! data for the low rank factorization
integer                          :: krank,iffactor
double precision, allocatable    :: dnus0(:),cosvals(:,:),sinvals(:,:)
double precision, allocatable    :: psivals0(:,:),avals0(:,:),cosvals0(:,:),sinvals0(:,:)
double precision, allocatable    :: r(:,:)

end type jacobi_expansion_data

contains





subroutine jacobi_expansion(eps,iffactor,dmax,da,db,expdata)
implicit double precision (a-h,o-z)

double precision                           :: da,db
type(jacobi_expansion_data), intent(out)  :: expdata

!
!  This routine constructs expansions of the phase and amplitude functions
!  for Jacobi's differential equation which vary with the argument t
!  and degree nu but are fixed with respect to the parameters da and db.
!
!  The resulting expansions allows for the evaluation of the Jacobi functions
!  on the interval (0,\pi) for nu between 1 and dmax (when the ifsmall flag
!  is set) or between 27 and dmax (when the ifsmall flag is not set).
!
!
!  Input parameters:
!   iffactor - a flag indicating whether to form a low rank factorizations which 
!      allows for the later application of the Jacobi transform or not
!
!      iffactor = 0    do not form the low rank factorizations used by the jacobi
!                      transform code
!      iffactor = 1    form the low rank factorizations
!
!    eps - precision for the low rank factorization (assuming it is formed)
!    dmax - specifies the largest degree for the factorization
!    (da,db) - the parameters in Jacobi's differential equation
!
!  Output parameters:
!    expdata - a data structure which stores all of the expansion data
!

double precision, allocatable :: rnorms(:),amatr(:,:),cd(:,:),whts(:),dnus(:)
integer, allocatable          :: ipivs(:)
data pi / 3.14159265358979323846264338327950288d0 /

double precision, allocatable :: dnus0(:),ts0(:),twhts0(:),xx(:),yy(:)
type(chebexps_data)           :: chebdata0


!
!  Setup the discretization scheme and allocate memory for the procedure
!

k1         = 16
k2         = 24

call chebexps(k1,expdata%chebdata1)
call chebexps(k2,expdata%chebdata2)

call  jacobi_expansion_cd(dmax,expdata%nintscd,expdata%cd)
call  jacobi_expansion_ab(expdata%cd(2,expdata%nintscd),expdata%nintsab,expdata%ab)

nintsab    = expdata%nintsab
nintscd    = expdata%nintscd
n          = k1*nintsab
m          = k2*nintscd

expdata%k1       = k1
expdata%k2       = k2
expdata%n        = n 
expdata%m        = m
expdata%da       = da 
expdata%db       = db
expdata%dmax     = dmax
expdata%iffactor = iffactor

if (iffactor .eq. 1) then
nextra          = k1
chebdata0=expdata%chebdata1
!call chebexps(nextra,chebdata0)
else
nextra = 0
endif

allocate(expdata%psivals(n,m))
allocate(  expdata%avals(n,m))
allocate(expdata%cosvals(n,m))
allocate(expdata%sinvals(n,m))

!
!  Construct the lists of frequencies 
!

allocate(ts0(nextra),twhts0(nextra))

a0 =  0.0d0
b0 =  6.0d0

do i=1,nextra
ts0(i)    = chebdata0%xs(i)*(b0-a0)/2 + (b0+a0)/2
twhts0(i) = chebdata0%whts(i)*(b0-a0)/2
end do


allocate(expdata%dnus(m),expdata%ts(n))

idx=0
do intab=1,expdata%nintsab
a = expdata%ab(1,intab)
b = expdata%ab(2,intab)
do i=1,expdata%k1
t                  = expdata%chebdata1%xs(i)*(b-a)/2 + (b+a)/2
idx                = idx+1
expdata%ts(idx)    = t
end do
end do

!
!  Construct the phase functions
!

do intcd=1,nintscd
c = expdata%cd(1,intcd)
d = expdata%cd(2,intcd)

do i=1,k2
dnu                = expdata%chebdata2%xs(i)*(d-c)/2 + (d+c)/2
idx                = i+(intcd-1)*k2
expdata%dnus(idx)  = dnu

call jacobi_phase(expdata%chebdata1,dnu,da,db,nintsab,expdata%ab,  &
  expdata%avals(1,idx),expdata%psivals(1,idx))

if (iffactor .eq. 1) then

expdata%cosvals(:,idx) = expdata%avals(:,idx)*cos(dnu*expdata%psivals(:,idx))
expdata%sinvals(:,idx) = expdata%avals(:,idx)*sin(dnu*expdata%psivals(:,idx))
! expdata%cosvals(:,idx) = expdata%avals(:,idx)*cos(expdata%psivals(:,idx))
! expdata%sinvals(:,idx) = expdata%avals(:,idx)*sin(expdata%psivals(:,idx))

endif

end do
end do

!
!  Build the low rank factorizations if the user so desires
!

if (iffactor .eq. 1) then
!allocate(amatr(4*n+2*nextra,m))
allocate(amatr(2*n+2*nextra,m))

amatr = 0

!amatr(1:n,       :) = expdata%psivals
!amatr(n+1:2*n,   :) = expdata%avals
!amatr(2*n+1:3*n, :) = expdata%cosvals
!amatr(3*n+1:4*n, :) = expdata%sinvals

amatr(1:n, :)     = expdata%cosvals
amatr(n+1:2*n, :) = expdata%sinvals
dnumax = expdata%cd(2,expdata%nintscd)

do i=1,nextra
t0                       = ts0(i)
wht0                     = twhts0(i)
do j=1,m
dnu                      = expdata%dnus(j)/dnumax
!amatr(4*n+i,j)           = cos(dnu*t0)*sqrt(wht0)
!amatr(4*n+i+nextra,j)    = sin(dnu*t0)*sqrt(wht0)

amatr(2*n+i,j)           = cos(dnu*t0)
!*sqrt(wht0)
amatr(2*n+i+nextra,j)    = sin(dnu*t0)
!*sqrt(wht0)

end do
end do

!
!  Scale by quadrature weights
!

allocate(whts(n))
idx1=1
idx2=k1
do int=1,expdata%nintsab
a = expdata%ab(1,int)
b = expdata%ab(2,int)
whts(idx1:idx2) = expdata%chebdata1%whts * (b-a)/2
idx1=idx1+k1
idx2=idx2+k1
end do

do j=1,m
amatr(1:n,j)       = amatr(1:n,j)      *sqrt(whts)
amatr(n+1:2*n,j)   = amatr(n+1:2*n,j)  *sqrt(whts)
!amatr(2*n+1:3*n,j) = amatr(2*n+1:3*n,j)*sqrt(whts)
!amatr(3*n+1:4*n,j) = amatr(3*n+1:4*n,j)*sqrt(whts)
end do

!
!  Form the interpolative decompositions
!


!!!! No factorization
! allocate(ipivs(m))
! krank = m
! do i=1,m
! ipivs(i) = i
! end do
! allocate(expdata%r(m,m))
! expdata%r = 0
! do i=1,m
! expdata%r(i,i) = 1
! end do

call idecomp_construct(eps,amatr,krank,ipivs,expdata%r)


allocate(expdata%dnus0(krank))
expdata%dnus0  = expdata%dnus(ipivs(1:krank))
expdata%krank  = krank

!allocate(expdata%psivals0(n,krank),expdata%avals0(n,krank))
allocate(expdata%cosvals0(n,krank),expdata%sinvals0(n,krank))

!expdata%psivals0 =  expdata%psivals(:,ipivs(1:krank))
!expdata%avals0   =    expdata%avals(:,ipivs(1:krank))
expdata%cosvals0 =  expdata%cosvals(:,ipivs(1:krank))
expdata%sinvals0 =  expdata%sinvals(:,ipivs(1:krank))


! print *,maxval(abs(matmul(expdata%psivals0,expdata%r)-expdata%psivals))
! print *,maxval(abs(matmul(expdata%avals0,expdata%r)-expdata%avals))
! print *,maxval(abs(matmul(expdata%cosvals0,expdata%r)-expdata%cosvals))
! print *,maxval(abs(matmul(expdata%sinvals0,expdata%r)-expdata%sinvals))

deallocate(expdata%cosvals,expdata%sinvals)
endif

end subroutine



subroutine jacobi_expansion_eval(expdata,nts,ts,ndnus,dnus,psivals,avals)
implicit double precision (a-h,o-z)

type(jacobi_expansion_data)        :: expdata
double precision, intent(in)        :: ts(nts),dnus(ndnus)
double precision, intent(out)       :: psivals(nts,ndnus),avals(nts,ndnus)

!
!  Evaluate the phase function at a user-specified collection of points.
!
!  WARNING: the list of points ts and degrees dnus must be sorted in ascending
!  order.
!
!  Input parameters:
!    expdata - the Jacobi expansion data structure generated by jacobi_expansion
!    nts - the number of values of the argument at which to evaluate the phase and
!      amplitude functions
!    ts - the list of arguments at which to evaluate the phase and amplitude
!      functions SORTED IN ASCENDING ORDER
!    ndnus - the number of degrees at which to evaluate the phase and amplitude
!       functions
!    dnus - the list of degrees at which to evaluate the phase and amplitude,
!      SORTED IN ASCENDING ORDER
!
!  Output parameters:
!    avals - this (nts,ndnus) user-supplied array will contain the desired
!      values  of the amplitude function
!   psivals - this (nts,ndnus) user-supplied array will contain the desired
!      values  of the phase function

!

data pi / 3.14159265358979323846264338327950288d0 /

double precision :: whtsab(expdata%k1), whtscd(expdata%k2)


da     = expdata%da
db     = expdata%db
eps0   = epsilon(0.0d0)
k1     = expdata%k1
k2     = expdata%k2
nintsab = expdata%nintsab
nintscd = expdata%nintscd
intab0 = 1


do i=1,nts
t = ts(i)

!
!  Find the interval containing t
!

do intab=intab0,nintsab-1
if (t .le. expdata%ab(2,intab)) exit
end do
a      = expdata%ab(1,intab)
b      = expdata%ab(2,intab)
intab0 = intab
t0     = (2*t - (b+a) ) /(b-a)

!
!  Calculate the values of the interpolation weights
!

inodeab = 0
dsign   = 1
do l=1,k1
diff      = t0 - expdata%chebdata1%xs(l)
whtsab(l) = dsign/diff 
dsign     = -dsign
if (abs(diff) .lt. eps0) then
inodeab=l
exit
endif
end do
whtsab(1)  = whtsab(1)/2
whtsab(k1) = whtsab(k1)/2
sumab      = sum(whtsab)


intcd0 = 1
do j=1,ndnus
dnu    = dnus(j)
p      = dnu + (da+db+1)/2
dconst = sqrt( (2*dnu+da+db+1)/pi )

!
!  Find the interval containing dnu
!

do intcd=intcd0,nintscd-1
if (dnu .le. expdata%cd(2,intcd)) exit
end do
c      = expdata%cd(1,intcd)
d      = expdata%cd(2,intcd)
intcd0 = intcd
dnu0   = (2*dnu - (d+c) ) /(d-c)

!
!  Construct the interpolation weightsarray
!

inodecd = 0
dsign  = 1
do l=1,k2
diff      = dnu0 - expdata%chebdata2%xs(l)
whtscd(l) = dsign/diff
dsign     = -dsign  
if (abs(diff) .lt. eps0) then
inodecd=l
exit
endif
end do
whtscd(1)  = whtscd(1)/2
whtscd(k2) = whtscd(k2)/2
sumcd      = sum(whtscd)


!
!  Handle the case in which t, dnu or both coincide with interpolation nodes
!

if (inodeab*inodecd .gt. 0) then
idx1         = inodeab + (intab-1)*k1
idx2         = inodecd + (intcd-1)*k2
psival       = expdata%psivals(idx1,idx2)
aval         = expdata%avals(idx1,idx2)
goto 1000
endif

if (inodeab .gt. 0) then
sum1 = 0
sum2 = 0
idx  = inodeab + (intab-1)*k1
do k=1,k2
sum1 = sum1 + expdata%psivals(idx, k + (intcd-1)*k2) * whtscd(k)
sum2 = sum2 + expdata%avals(idx, k + (intcd-1)*k2) * whtscd(k)
end do
psival       = sum1/(sumcd)
aval         = sum2/(sumcd)
goto 1000
endif


if (inodecd .gt. 0) then
sum1 = 0
sum2 = 0
idx  = inodecd + (intcd-1)*k2
do l=1,k1
sum1 = sum1 + expdata%psivals(l + (intab-1)*k1,idx) * whtsab(l)
sum2 = sum2 + expdata%avals(l + (intab-1)*k1,idx) * whtsab(l)
end do
psival      = sum1/(sumab)
aval        = sum2/(sumab)
goto 1000
endif

!
!  Compute the sums 
!

sum1 = 0
sum2 = 0

do l=1,k1
do k=1,k2
sum1 = sum1 + expdata%psivals(l + (intab-1)*k1, k + (intcd-1)*k2) * whtsab(l)*whtscd(k)
sum2 = sum2 + expdata%avals(l + (intab-1)*k1, k + (intcd-1)*k2) * whtsab(l)*whtscd(k)
end do
end do

psival = sum1/(sumab*sumcd)
aval   = sum2/(sumab*sumcd)

1000 continue

psivals(i,j) = (psival+t)*dnu
avals(i,j)   = aval*dconst

end do

end do


end subroutine



subroutine jacobi_expansion_size(expdata,dsize)
implicit double precision (a-h,o-z)
type(jacobi_expansion_data), intent(in)  :: expdata

!
!  Return the size of the data stored in a jacobi_expansion_data structure.
!
!  Input parameters:
!   expdata - the jacobi expansion structure
!
!  Output parameters:
!    dsize - the size, in megabytes, of the data stored in the structure
!

dsize = 32 + 24 + 8*(size(expdata%ab) + size(expdata%cd) )
dsize = dsize   + 8*(size(expdata%psivals) + size(expdata%avals))
dsize = dsize   + 8*(size(expdata%ts) + size(expdata%dnus))

if (expdata%iffactor .eq. 1) then
dsize = dsize + 8*(size(expdata%dnus0) + size(expdata%cosvals0))
dsize = dsize + 8*(size(expdata%sinvals0) + size(expdata%r))
endif

dsize = dsize/(1024.0d0*1024.0d0)
end subroutine



subroutine jacobi_expansion_test(expdata)
implicit double precision (a-h,o-z)

double precision, allocatable :: ts(:),dnus(:),avals(:,:),psivals(:,:),vals(:,:),vals0(:,:)
double precision, allocatable :: errs1(:),errs2(:)
double precision, allocatable :: ab(:,:),psivals0(:),avals0(:),psivals00(:),avals00(:),amatr(:,:)
type(chebexps_data)           :: chebdata
type(jacobi_expansion_data)  :: expdata

!
!  Test that the expansion evaluates the phase and amplitude correctly.
!
!  Input parameters:
!    expdata - a jacobi_expansion_data structure generated by jacobi_expansion
!
!  Output parameters:
!    none
!

da   = expdata%da
db   = expdata%db
pi   = acos(-1.0d0)
dmax = expdata%dmax

nts   = 200
ndnus = 200

allocate(ts(nts),dnus(ndnus),psivals(nts,ndnus),avals(nts,ndnus))
allocate(avals0(nts),psivals0(nts),errs1(ndnus),errs2(ndnus))


a = expdata%ab(1,1)
b = expdata%ab(2,expdata%nintsab)

do i=1,nts
!call random_number(dd)
dd = (i)/(nts+1.0d0)
ts(i) = a + dd*(b-a)
end do

c = expdata%cd(1,1)
d = expdata%cd(2,1)

! do i=1,10
! !call random_number(dd)
! dd      = (i-1.0d0)/(ndnus-1.0d0)
! dnus(i) = c + dd*(d-c)
! end do

c = expdata%cd(2,1)
d = expdata%cd(2,expdata%nintscd)

do i=1,ndnus
!call random_number(dd)
dd      = (i-1.0d0)/(ndnus-1.0d0)
dnus(i) = exp(log(c) + (log(d) - log(c) ) *dd)
end do

call quicksort(nts,ts)
call quicksort(ndnus,dnus)

call prin2("in jacobi_test_exp1, ts = ",ts)
call prin2("in jacobi_test_exp1, dnus = ",dnus)

call elapsed(t1)
call jacobi_expansion_eval(expdata,nts,ts,ndnus,dnus,psivals,avals)
call elapsed(t2)

call prin2("average evaluation time = ",(t2-t1)/(nts*ndnus))

!
!  Check on the errors in the phase and amplitude functions
!

k = 30
call chebexps(k,chebdata)

! nints = 50
nints = expdata%nintsab
allocate(ab(2,nints),avals00(k*nints),psivals00(k*nints))
call jacobi_phase_disc(nints,ab)

do j=1,ndnus
dnu = dnus(j)
p   = dnu+(da+db+1)/2
call jacobi_phase(chebdata,dnu,da,db,nints,ab,avals00,psivals00)
call jacobi_phase_eval(chebdata,dnu,da,db,nints,ab,avals00,psivals00,nts,ts,avals0,psivals0)

errs1(j) = maxval(abs((avals0-avals(:,j)) / (abs(avals0)+1)))
errs2(j) = maxval(abs(psivals0-psivals(:,j))/(abs(psivals0)+1))

end do

call prin2("avals errs = ",errs1)
call prin2("psivals errs = ",errs2)

deallocate(ts,dnus,psivals,avals,psivals0,avals0)

end subroutine



subroutine jacobi_expansion_cd(dmax,nintscd,cd)
implicit double precision (a-h,o-z)

double precision, allocatable :: cd(:,:),cd0(:,:)

allocate(cd0(2,1000))

nintscd        =  0




nintscd        =  nintscd+1
cd0(1,nintscd) =  27.0d0
cd0(2,nintscd) =  81.0d0


do while (cd0(2,nintscd) .lt. dmax)
nintscd        = nintscd+1
cd0(1,nintscd) =  cd0(2,nintscd-1)
cd0(2,nintscd) =  cd0(2,nintscd-1)*3.0d0

end do

cd0(2,nintscd) = min(dmax,cd0(2,nintscd))


allocate(cd(2,nintscd))
cd = cd0(:,1:nintscd)

end subroutine



subroutine jacobi_expansion_ab(dmax,nintsab,ab)
implicit double precision (a-h,o-z)

double precision, allocatable :: ab(:,:)
data pi / 3.14159265358979323846264338327950288d0 /

dd         = min(1/dmax,0.025d0)
dd         = dd * 2/pi
dd         = min(0.01d0,dd)
dd         = log(dd)/log(2.0d0)
nints      = ceiling(-dd)
nintsab    = 2*nints

allocate(ab(2,nintsab))
call jacobi_phase_disc(nintsab,ab)

end subroutine


subroutine jacobi_expansion_write(iw,expdata)
implicit double precision (a-h,o-z)

type(jacobi_expansion_data)  :: expdata

call write_chebexps(iw,expdata%chebdata1)
call write_chebexps(iw,expdata%chebdata2)

write (iw,"(I8.8)")       expdata%k1
write (iw,"(I8.8)")       expdata%k2
write (iw,"(I8.8)")       expdata%nintsab
write (iw,"(I8.8)")       expdata%nintscd
write (iw,"(I8.8)")       expdata%n
write (iw,"(I8.8)")       expdata%m
write (iw,"(D30.20)")     expdata%da
write (iw,"(D30.20)")     expdata%db
write (iw,"(D30.20)")     expdata%dmax

write (iw,"(D30.20)")     expdata%ab
write (iw,"(D30.20)")     expdata%cd
write (iw,"(D30.20)")     expdata%psivals
write (iw,"(D30.20)")     expdata%avals
write (iw,"(D30.20)")     expdata%ts
write (iw,"(D30.20)")     expdata%dnus
write (iw,"(I8.8)")       expdata%iffactor

if (expdata%iffactor .eq. 1) then
write (iw,"(I8.8)")       expdata%krank
write (iw,"(D30.20)")     expdata%dnus0
write (iw,"(D30.20)")     expdata%psivals0
write (iw,"(D30.20)")     expdata%avals0
write (iw,"(D30.20)")     expdata%cosvals0
write (iw,"(D30.20)")     expdata%sinvals0
write (iw,"(D30.20)")     expdata%r
endif

end subroutine



subroutine jacobi_expansion_read(iw,expdata)
implicit double precision (a-h,o-z)

type(jacobi_expansion_data), intent(out)  :: expdata

call read_chebexps(iw,expdata%chebdata1)
call read_chebexps(iw,expdata%chebdata2)

read (iw,"(I8.8)")       expdata%k1
read (iw,"(I8.8)")       expdata%k2
read (iw,"(I8.8)")       expdata%nintsab
read (iw,"(I8.8)")       expdata%nintscd
read (iw,"(I8.8)")       expdata%n
read (iw,"(I8.8)")       expdata%m
read (iw,"(D30.20)")     expdata%da
read (iw,"(D30.20)")     expdata%db
read (iw,"(D30.20)")     expdata%dmax

allocate(expdata%ab(2,expdata%nintsab))
allocate(expdata%cd(2,expdata%nintscd))
allocate(expdata%psivals(expdata%n,expdata%m))
allocate(expdata%avals(expdata%n,expdata%m))

allocate(expdata%ts(expdata%n))
allocate(expdata%dnus(expdata%m))

read (iw,"(D30.20)")     expdata%ab
read (iw,"(D30.20)")     expdata%cd
read (iw,"(D30.20)")     expdata%psivals
read (iw,"(D30.20)")     expdata%avals
read (iw,"(D30.20)")     expdata%ts
read (iw,"(D30.20)")     expdata%dnus
read (iw,"(I8.8)")       expdata%iffactor

if (expdata%iffactor .eq. 1) then

read  (iw,"(I8.8)")       expdata%krank
allocate(expdata%dnus0(expdata%krank))
allocate(expdata%psivals0(expdata%n,expdata%krank))
allocate(expdata%avals0(expdata%n,expdata%krank))
allocate(expdata%cosvals0(expdata%n,expdata%krank))
allocate(expdata%sinvals0(expdata%n,expdata%krank))
allocate(expdata%r(expdata%krank,expdata%m))

read (iw,"(D30.20)")     expdata%dnus0
read (iw,"(D30.20)")     expdata%psivals0
read (iw,"(D30.20)")     expdata%avals0
read (iw,"(D30.20)")     expdata%cosvals0
read (iw,"(D30.20)")     expdata%sinvals0
read (iw,"(D30.20)")     expdata%r
endif

end subroutine

end module
