program test_jacobi_quad
use utils
use chebyshev
implicit double precision (a-h,o-z)

double precision, allocatable :: xs(:),xwhts(:)
double precision, allocatable :: xs0(:),xwhts0(:)

double precision, allocatable :: ts(:),twhts(:)
double precision, allocatable :: vals(:)

double precision, allocatable :: psivals1(:,:),psivals2(:,:),avals1(:,:),avals2(:,:)
double precision, allocatable :: vals1(:),vals2(:),ab(:,:)
double precision, allocatable :: avals0(:),psivals0(:)
type(chebexps_data)           :: chebdata


pi = acos(-1.0d0)
n  = 1001

da = 0.25d0
db = 0.00d0

eps0 = epsilon(0.0d0)

allocate(xs0(n),xwhts0(n))
allocate(xs(n),xwhts(n))

call elapsed(t1)
call jacobi_quad(n,da,db,xs,xwhts)
call elapsed(t2)
call prin2("quad time = ",t2-t1)
call prin2("error in integral = ",sum(cos(xs)**2*xwhts)-1.40745188029488069686727262081d0)




!
!  Make the modified quadrature rule
!
allocate(ts(n),twhts(n))
call jacobi_quad_mod(n,da,db,ts,twhts)


sum1 = 0
nu1  = 11
nu2  = 10
dnu1 = nu1
dnu2 = nu2

k = 20
call chebexps(k,chebdata)
nints = 50
allocate(ab(2,nints))
allocate(avals1(k,nints),psivals1(k,nints),vals1(n))
allocate(avals2(k,nints),psivals2(k,nints),vals2(n))
allocate(avals0(n),psivals0(n))

call jacobi_phase_disc(nints,ab)
call jacobi_phase(chebdata,dnu1,da,db,nints,ab,avals1,psivals1)
call jacobi_phase(chebdata,dnu2,da,db,nints,ab,avals2,psivals2)

call jacobi_phase_eval(chebdata,dnu1,da,db,nints,ab,avals1,psivals1,n,ts,avals0,psivals0)
vals1 = cos(psivals0)*avals0

call jacobi_phase_eval(chebdata,dnu2,da,db,nints,ab,avals2,psivals2,n,ts,avals0,psivals0)
vals2 = cos(psivals0)*avals0


vals1 = vals1 * sqrt(twhts)
vals2 = vals2 * sqrt(twhts)
sum1 = sum(vals1*vals2)

call prin2("inner product = ",sum1)



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

x    = cos(t)

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
