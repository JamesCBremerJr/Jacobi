!
!  Conduct the experiments in Section~7.1 of the preprint
!
!    James Bremer and Haizhao Yang,  "Fast algorithms for Jacobi polynomials 
!    via nonoscillatory phase functions." 
!
!
!

program jacobi_experiment1
use utils
use chebyshev
use jacobi_exp
implicit double precision (a-h,o-z)

k1 = 6
k2 = 24
call  asymptotic_table(k1,k2)  ! tableasym.tex and tablesize.tex
call  graphs(k1,k2)            ! construct the two graphs

k1 = 5
k2 = 15
call  recurrence_table(k1,k2)  ! tablerec.tex

end program



subroutine graphs(k1,k2)
use utils
implicit double precision (a-h,o-z)


double precision, allocatable :: times(:,:),errs(:),dsizes(:),dmaxs(:)
double precision, allocatable :: times2(:,:),times3(:,:),times4(:,:)
double precision, allocatable :: errs2(:),errs3(:),errs4(:)


allocate(times(3,k1:k2),errs(k1:k2),dsizes(k1:k2),dmaxs(k1:k2))
allocate(times2(3,k1:k2),times3(3,k1:k2),times4(3,k1:k2))
allocate(errs2(k1:k2),errs3(k1:k2),errs4(k1:k2))



da       = 0.49d0
db       = 0.35d0
eps      = 1.0d-12

if (k2 .eq. 20) then
themax = 10.0d0**6
elseif (k2 .eq. 24) then
themax = 10.0d0**7
else
themax = 10.0d0**8
endif


do k=k1,k2
dmax     = 2**k
dmax     = max(100.0d0,dmax)
dmax     = min(themax,dmax)
dmaxs(k) = dmax
call test_expansion_asym(da,db,dmax,times(:,k),errs(k),dsizes(k))
end do

da       =-0.25d0
db       = 0.49d0

do k=k1,k2
dmax     = 2**k
dmax     = max(100.0d0,dmax)
dmax     = min(themax,dmax)
dmaxs(k) = dmax
call test_expansion_asym(da,db,dmax,times2(:,k),errs2(k),dsizes(k))
end do



da       = 0.49d0
db       =-0.40d0

do k=k1,k2
dmax     = 2**k
dmax     = max(100.0d0,dmax)
dmax     = min(themax,dmax)
dmaxs(k) = dmax
call test_expansion_asym(da,db,dmax,times3(:,k),errs3(k),dsizes(k))
end do

da       =-0.25d0
db       =-0.40d0

do k=k1,k2
dmax     = 2**k
dmax     = max(100.0d0,dmax)
dmax     = min(themax,dmax)
dmaxs(k) = dmax
call test_expansion_asym(da,db,dmax,times4(:,k),errs4(k),dsizes(k))
end do

m = k2-k1+1
call prin2("dmaxs = ",dmaxs)

iw = 20
open(iw,FILE='graph_precomp.py')
call pyplot_begin(iw,istatus)
call pyplot_xlogscale(istatus)
call pyplot_ylogscale(istatus)
call pyplot_xlabel(istatus,"$N_{\mbox{max}}$")
call pyplot_ylabel(istatus,"phase function construction time (s)")
call pyplot_add_function(istatus,1,"a= 0.49, b = 0.35",m,dmaxs,times(1,:))
call pyplot_add_function(istatus,2,"a=-0.25, b = 0.49",m,dmaxs,times2(1,:))
call pyplot_add_function(istatus,3,"a= 0.49, b = -0.40",m,dmaxs,times3(1,:))
call pyplot_add_function(istatus,4,"a=-0.25, b = -0.40",m,dmaxs,times4(1,:))
call pyplot_end(istatus,"graph_precomp.pdf")
close(iw)


iw = 21
open(iw,FILE='graph_everrs.py')
call pyplot_begin(iw,istatus)
call pyplot_xlogscale(istatus)
call pyplot_ylogscale(istatus)
call pyplot_xlabel(istatus,"$N_{\mbox{max}}$")
call pyplot_ylabel(istatus,"maximum absolute error")
call pyplot_add_function(istatus,1,"a= 0.49, b = 0.35",m,dmaxs,errs)
call pyplot_add_function(istatus,2,"a=-0.25, b = 0.49",m,dmaxs,errs2)
call pyplot_add_function(istatus,3,"a= 0.49, b = -0.40",m,dmaxs,errs3)
call pyplot_add_function(istatus,4,"a=-0.25, b = -0.40",m,dmaxs,errs4)
call pyplot_end(istatus,"graph_everr.pdf")
close(iw)

end subroutine






subroutine jacobi_recurrence(n,da,db,t,vals)
implicit double precision (a-h,o-z)
integer                    :: n
double precision           :: t,da,db
double precision           :: vals(0:n)

!
!  Evaluate the functions (1) for dnu=0,1,...,n at a specified point t
!  using the well-known recurrence relations.
!
data pi / 3.14159265358979323846264338327950288d0 /
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






subroutine asymptotic_table(k1,k2)
use utils
implicit double precision (a-h,o-z)

!
!  Perform the comparison with asymptotic expansions
!

double precision, allocatable :: times(:,:),errs(:),dsizes(:),dmaxs(:)


!
!  Build and test an expansion
!

pi       = acos(-1.0d0)
da       = -0.25d0
db       = 1.0d0/3.00d0

allocate(times(3,k1:k2),errs(k1:k2),dsizes(k1:k2),dmaxs(k1:k2))

do k=k1,k2
dmax     = 2**k
dmax     = max(100.0d0,dmax)
dmaxs(k) = dmax
call test_expansion_asym(da,db,dmax,times(:,k),errs(k),dsizes(k))
end do



 !!!!!!!!!! construct the LaTeX table

iw2 = 21
open(iw2,FILE='tableasym.tex')

write(iw2,*) "\begin{tabular}{lccccc}"
write(iw2,*) "\toprule"
write(iw2,*) "$N_{\mbox{\tiny max}}$ & Phase function  & Avg. eval time & Avg. eval time& Largest absolute & Expansion size\\"
write(iw2,*) "&construction time & algorithm & asymptotic &   error & (MB)\\"
write(iw2,*) "&  & of this paper & expansions  &   &\\"

!write(iw2,*) "&&   & asymptotic expansions          & \\"

write(iw2,*) "\midrule"

do k=k1,k2
n = dmaxs(k)
! call write_table_integer_power2(iw2,n)
call write_table_integer_sep(iw2,n)
call write_table_next(iw2)
call write_table_double(iw2,times(1,k))
call write_table_next(iw2)
call write_table_double(iw2,times(2,k))
call write_table_next(iw2)
call write_table_double(iw2,times(3,k))
call write_table_next(iw2)
call write_table_double(iw2,errs(k))
call write_table_next(iw2)
call write_table_double(iw2,dsizes(k))
call write_table_nextline(iw2)

end do

write (iw2,*) "\bottomrule"
write (iw2,*) "\end{tabular}"

close(iw2)


end subroutine


subroutine test_expansion_asym(da,db,dmax,times,derr,dsize)
use utils
use chebyshev
use jacobi_exp
implicit double precision (a-h,o-z)

type(jacobi_expansion_data)   :: expdata
double precision              :: times(3)
double precision, allocatable :: dnus(:),ts(:),psivals(:,:),avals(:,:)
double precision, allocatable :: polvals(:,:),polvals0(:,:)


eps      = 1.0d-13
iffactor = 0
pi       = acos(-1.0d0)

ndnus = 200
nts   = 200

allocate(ts(nts),dnus(ndnus),psivals(nts,ndnus),avals(nts,ndnus))
allocate(polvals(nts,ndnus),polvals0(nts,ndnus))

call prin2("dmax = ",dmax)

call elapsed(t1)
call jacobi_expansion(eps,iffactor,dmax,da,db,expdata)
call elapsed(t2)
call prin2("expansion time = ",t2-t1)
times(1) = t2-t1

call jacobi_expansion_size(expdata,dsize)
call prin2("expansion size (MB) = ",dsize)


a = expdata%ab(1,1)
b = pi/2

do i=1,nts
call random_number(dd)
ts(i) = a + dd*(b-a)
end do


c = 100.0d0
d = expdata%cd(2,expdata%nintscd)

do i=1,ndnus
call random_number(dd)
dnus(i) = c + (d-c)*dd
end do


call quicksort(nts,ts)
call quicksort(ndnus,dnus)
call prin2("ts = ",ts)
call prin2("dnus = ",dnus)

call elapsed(t1)
call jacobi_expansion_eval(expdata,nts,ts,ndnus,dnus,psivals,avals)
polvals = cos(psivals)*avals
call elapsed(t2)
dtime = (t2-t1)/(ndnus*nts)


times(2) = dtime

call elapsed(t1)
do j=1,ndnus
dnu = dnus(j)
do i=1,nts
call jacobi_asym_eval(dnu,da,db,ts(i),polvals0(i,j))

end do
dconst        = sqrt((2*dnu+da+db+1)/pi)
polvals0(:,j) = polvals0(:,j)*dconst
end do

call elapsed(t2)
dtime2 = (t2-t1)/(ndnus*nts)

times(3) = dtime2

derr = maxval(abs(polvals-polvals0))
call prin2("maximum absolute error = ",derr)
call prin2("average eval time  = ",dtime)
call prin2("average asym time = ",dtime2)
call prina("")


end subroutine



subroutine recurrence_table(k1,k2)
use utils
implicit double precision (a-h,o-z)

!
!  Perform the comparison with recurrence relations
!

double precision, allocatable :: times(:,:),errs(:),dsizes(:),dmaxs(:)
double precision, allocatable :: times2(:,:),times3(:,:),times4(:,:)
double precision, allocatable :: errs2(:),errs3(:),errs4(:)


!
!  Build and test an expansion
!

pi       = acos(-1.0d0)
da       = 0.25d0
db       = -1.0d0/3.00d0


allocate(times(3,k1:k2),errs(k1:k2),dsizes(k1:k2),dmaxs(k1:k2))
allocate(times2(3,k1:k2),times3(3,k1:k2),times4(3,k1:k2))
allocate(errs2(k1:k2),errs3(k1:k2),errs4(k1:k2))

do k=k1,k2
dmax     = 2**k
dmaxs(k) = dmax
call test_expansion_rec(da,db,dmax,times(:,k),errs(k),dsizes(k))
end do


 !!!!!!!!!! construct the LaTeX table

iw2 = 21
open(iw2,FILE='tablerec.tex')

write(iw2,*) "\begin{tabular}{lcccc}"
write(iw2,*) "\toprule"
write(iw2,*) "$N_{\mbox{\tiny max}}$ & Phase function  & Average evaluation time & Average evaluation time & Largest absolute \\"
write(iw2,*) "& construction time & algorithm of this paper& recurrence relations       & error\\"
!write(iw2,*) "&&   & asymptotic expansions          & \\"

write(iw2,*) "\midrule"

do k=k1,k2
n = dmaxs(k)
! call write_table_integer_power2(iw2,n)
call write_table_integer_sep(iw2,n)
call write_table_next(iw2)
call write_table_double(iw2,times(1,k))
call write_table_next(iw2)
call write_table_double(iw2,times(2,k))
call write_table_next(iw2)
call write_table_double(iw2,times(3,k))
call write_table_next(iw2)
call write_table_double(iw2,errs(k))
call write_table_nextline(iw2)
end do

write (iw2,*) "\bottomrule"
write (iw2,*) "\end{tabular}"

close(iw2)

stop

end subroutine


subroutine test_expansion_rec(da,db,dmax,times,derr,dsize)
use utils
use chebyshev
use jacobi_exp
implicit double precision (a-h,o-z)

type(jacobi_expansion_data)   :: expdata
double precision              :: times(3)
double precision, allocatable :: dnus(:),ts(:),psivals(:,:),avals(:,:)
double precision, allocatable :: polvals(:,:),polvals0(:,:),xx(:)


eps      = 1.0d-13
iffactor = 0
pi       = acos(-1.0d0)

ndnus = 200
nts   = 200

allocate(ts(nts),dnus(ndnus),psivals(nts,ndnus),avals(nts,ndnus))
allocate(polvals(nts,ndnus),polvals0(nts,ndnus))

call prin2("dmax = ",dmax)

call elapsed(t1)
call jacobi_expansion(eps,iffactor,dmax,da,db,expdata)
call elapsed(t2)
call prin2("expansion time = ",t2-t1)
times(1) = t2-t1

call jacobi_expansion_size(expdata,dsize)
call prin2("expansion size (MB) = ",dsize)


a = expdata%ab(1,1)
b = pi/2

do i=1,nts
call random_number(dd)
ts(i) = a + dd*(b-a)
end do


c = 27.0d0
d = expdata%cd(2,expdata%nintscd)

do i=1,ndnus
call random_number(dd)
dnus(i) = c + (d-c)*dd
!dnus(i) = exp(log(c) + (log(d) - log(c) ) *dd)
end do

dnus = int(dnus)

call quicksort(nts,ts)
call quicksort(ndnus,dnus)
call prin2("ts = ",ts)
call prin2("dnus = ",dnus)

call elapsed(t1)
call jacobi_expansion_eval(expdata,nts,ts,ndnus,dnus,psivals,avals)
polvals = cos(psivals)*avals
call elapsed(t2)
dtime = (t2-t1)/(ndnus*nts)


times(2) = dtime

call elapsed(t1)
do j=1,ndnus
dnu = dnus(j)
nu  = dnu
allocate(xx(0:nu))

do i=1,nts
call jacobi_recurrence(nu,da,db,ts(i),xx)
polvals0(i,j) = xx(nu)

end do
deallocate(xx)
end do

call elapsed(t2)
dtime2 = (t2-t1)/(ndnus*nts)

times(3) = dtime2

derr = maxval(abs(polvals-polvals0))
call prin2("maximum absolute error = ",derr)
call prin2("average eval time  = ",dtime)
call prin2("average asym time = ",dtime2)

call prina("")


end subroutine

