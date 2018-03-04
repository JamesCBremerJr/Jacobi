program test_jacobi_solve
use utils
use chebyshev
implicit double precision (a-h,o-z)

type(chebexps_data)           :: chebdata
double precision, allocatable :: ab(:,:)
double precision, allocatable :: psivals(:),avals(:)
double precision, allocatable :: ts(:),avals0(:),psivals0(:),polvals(:),polvals0(:),xx(:)


k   = 20
call chebexps(k,chebdata)
pi  = acos(-1.0d0)

!
! WARNING: the order must be >= 100 or the test code will loose accuracy
!

dnu = 1000.1d0
da  = 0.250d0
db  =-0.40d0
p   = dnu + (da+db+1)/2


dd      = min(0.025,1/dnu)
dd      = dd * 2/pi
dd      = log(dd)/log(2.0d0)
nints   = ceiling(-dd)*2

allocate(ab(2,nints))

call jacobi_phase_disc(nints,ab)
call prin2("before jacobi_phase, dnu = ",dnu)
call prini("before jacobi_phase, nints = ",nints)
call prin2("before jacobi_phase, ab = ",ab)

allocate(psivals(k*nints),avals(k*nints))

call elapsed(t1)
call jacobi_phase(chebdata,dnu,da,db,nints,ab,avals,psivals)
call elapsed(t2)
call prin2("time to construct phase = ",(t2-t1))



! t0 = ab(1,1)

! do int=1,nints
! a = ab(1,int)
! b = ab(2,int)
! do i=1,k
! t    = (b-a)/2 * chebdata%xs(i) + (b+a)/2

! val0 = psivals(1) + p*(t-t0)
! val1 = psivals(1) + dnu*(t-t0)

! print *,psivals(i+(int-1)*k)-val0


! end do
! end do
! stop



nts = 100
allocate(ts(nts),avals0(nts),psivals0(nts),polvals(nts),polvals0(nts))

a = ab(1,1)
b = pi/2         ! limitations of test code
!b = ab(2,nints)

do i=1,nts
call random_number(dd)
ts(i) = a + (b-a)*dd
end do

call quicksort(nts,ts)
call prin2("ts = ",ts)

call elapsed(t1)
call jacobi_phase_eval(chebdata,dnu,da,db,nints,ab,avals,psivals,nts,ts,avals0,psivals0)
call elapsed(t2)
call prin2("average eval time = ",(t2-t1)/nts)
polvals0 = cos(psivals0)*avals0

nu = dnu
allocate(xx(0:nu))

call elapsed(t1)
do i=1,nts
if (dnu .lt. 101) then
call jacobi_recurrence(nu,da,db,ts(i),xx)
polvals(i) = xx(nu)
else
call jacobi_asym_eval(dnu,da,db,ts(i),polvals(i))
endif

end do

if (dnu .gt. 101) then
dconst      = sqrt((2*dnu+da+db+1)/pi)
polvals     = polvals  * dconst
endif

call elapsed(t2)

call prin2("errs = ",abs(polvals-polvals0))
call prin2("asym exp time = ",(t2-t1)/nts)
call prin2("max error = ",maxval(abs(polvals-polvals0)))

end program





subroutine jacobi_recurrence(n,da,db,t,vals)
implicit double precision (a-h,o-z)
integer                    :: n
double precision           :: t,da,db
double precision           :: vals(0:n)

!
!  Evaluate the functions (1) for dnu=0,1,...,n at a specified point t
!  using the well-known recurrence relations.
!

dnu = n
x   = cos(t)

vals(0) = sqrt(0.2d1**(-0.1d1-0.1d1*da-0.1d1*db)*(0.1d1+da+db))*sqrt(Gamma(0.1d1  &
+da+db)/(Gamma(0.1d1+da)*Gamma(0.1d1+db)))

vals(1) = (sqrt(0.2d1**(-0.1d1-0.1d1*da-0.1d1*db)*(0.3d1+da+db))*(da-0.1d1*db+(2  &
+da+db)*x)*sqrt(Gamma(0.2d1+da+db)/(Gamma(0.2d1+da)*Gamma(0.2d1+  &
db))))/0.2d1


do i=2,n

dd1 = (sqrt((-0.1d1+da+db+0.2d1*i)*(0.1d1+da+db+0.2d1*i))*(0.4d1*(-1+da+  &
db)*i*x+0.4d1*i**2*x+(da+db)*(da-0.1d1*db+(-2+da+  &
db)*x)))/(0.2d1*Sqrt((i*(da+i)*(db+i))/(da+db+i))*(da+db+i)*(-2+da+db+0.2d1*i))

dd2 =(2**((da+db)/0.2d1)*(-1+da+i)*(-1+db+i)*(da+db+0.2d1*i))/(i*(da+db+  &
i)*Sqrt(-3+da+db+0.2d1*i)*(-2+da+db+0.2d1*i)*Sqrt((2**(da+db)*(-1+da+  &
i)*(da+i)*(-1+db+i)*(db+i))/((-1+i)*i*(-1+da+db+i)*(da+db+i)*(1+da+db+&
0.2d1*i))))

vals(i) = dd1*vals(i-1) - dd2*vals(i-2)

end do

!
!  Scale by r(t) now
!

rval = 2.0d0**((1+da+db)/2) * cos(t/2)**(db+0.5d0) * sin(t/2)**(da+0.5d0)
vals = vals * rval

!
!  Wronskian normalization.
!

! vals = vals * sqrt(pi / (2*dnu+da+db+1))

end subroutine

