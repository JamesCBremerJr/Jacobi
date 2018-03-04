!
!  Conduct the experiments in Section~7.3 of the preprint
!
!    James Bremer and Haizhao Yang,  "Fast algorithms for Jacobi polynomials 
!    via nonoscillatory phase functions."  
!


program jacobi_experiment3

use utils
use chebyshev
use idecomp
use jacobi_exp
use jacobi_transform

implicit double precision (a-h,o-z)

i1 = 5
i2 = 27

call comparison_slevinsky()
call error_test(i1,i2)
call rank_test(i1,i2)

end program


subroutine rank_test(i1,i2)
use utils
implicit double precision (a-h,o-z)

double precision, allocatable :: dnus1(:),times1(:,:),errs1(:),dsizes1(:)
double precision, allocatable :: dnus2(:),times2(:,:),errs2(:),dsizes2(:)
double precision, allocatable :: dnus3(:),times3(:,:),errs3(:),dsizes3(:)
double precision, allocatable :: dnus4(:),times4(:,:),errs4(:),dsizes4(:)
double precision, allocatable :: dkranks1(:),dkranks2(:),dkranks3(:),dkranks4(:)

integer, allocatable          :: kranks1(:)
integer, allocatable          :: kranks2(:)
integer, allocatable          :: kranks3(:)
integer, allocatable          :: kranks4(:)




allocate(dnus1(i1:i2),times1(5,i1:i2),errs1(i1:i2),dsizes1(i1:i2),kranks1(i1:i2))
allocate(dnus2(i1:i2),times2(5,i1:i2),errs2(i1:i2),dsizes2(i1:i2),kranks2(i1:i2))
allocate(dnus3(i1:i2),times3(5,i1:i2),errs3(i1:i2),dsizes3(i1:i2),kranks3(i1:i2))
allocate(dnus4(i1:i2),times4(5,i1:i2),errs4(i1:i2),dsizes4(i1:i2),kranks4(i1:i2))


da      = 0.49d0
db      = 0.20d0
decay   = 1.0d0
call comp_err(decay,i1,i2,da,db,times1,dnus1,errs1,dsizes1,kranks1)

da      =-0.25d0
db      = 0.00d0
decay   = 1.0d0
call comp_err(decay,i1,i2,da,db,times2,dnus2,errs2,dsizes2,kranks2)

da      =-0.40d0
db      =-0.40d0
decay   = 1.0d0
call comp_err(decay,i1,i2,da,db,times3,dnus3,errs3,dsizes3,kranks3)


da      = 0.25d0
db      =-0.20d0
decay   = 1.0d0
call comp_err(decay,i1,i2,da,db,times4,dnus4,errs4,dsizes4,kranks4)


allocate(dkranks1(i1:i2))
allocate(dkranks2(i1:i2))
allocate(dkranks3(i1:i2))
allocate(dkranks4(i1:i2))

dkranks1 = kranks1(i1:i2)
dkranks2 = kranks2(i1:i2)
dkranks3 = kranks3(i1:i2)
dkranks4 = kranks4(i1:i2)

! build the graph
iw2 = 21
open(iw2,FILE='graph_transranks.py')
call pyplot_begin(iw2,istatus)
m = i2-i1+1

call pyplot_add_function(istatus,1,"a= 0.49, b= 0.20",m,dnus1,dkranks1)
call pyplot_add_function(istatus,2,"a=-0.25, b= 0.00",m,dnus2,dkranks2)
call pyplot_add_function(istatus,2,"a=-0.40, b=-0.40",m,dnus2,dkranks3)
call pyplot_add_function(istatus,2,"a= 0.25, b=-0.20",m,dnus2,dkranks4)
call pyplot_xlogscale(istatus)
call pyplot_ylogscale(istatus)
call pyplot_xlabel(istatus,"N")
call pyplot_ylabel(istatus,"numerical rank")
call pyplot_end(istatus,"graph_transranks.pdf")
close(iw2)


! iw2 = 21
! open(iw2,FILE='graph_transranks2.py')
! call pyplot_begin(iw2,istatus)
! m = i2-i1+1

! call pyplot_add_function(istatus,1,"a= 0.49, b= 0.20",m,dnus1,dkranks1)
! call pyplot_add_function(istatus,2,"a=-0.25, b= 0.00",m,dnus2,dkranks2)
! call pyplot_add_function(istatus,2,"a=-0.40, b=-0.40",m,dnus2,dkranks3)
! call pyplot_add_function(istatus,2,"a= 0.25, b=-0.20",m,dnus2,dkranks4)
! call pyplot_xlogscale(istatus)
! !call pyplot_ylogscale(istatus)
! call pyplot_xlabel(istatus,"N")
! call pyplot_ylabel(istatus,"numerical rank")
! call pyplot_end(istatus,"graph_transranks2.png")
! close(iw2)


end subroutine


subroutine error_test(i1,i2)
use utils
implicit double precision (a-h,o-z)

double precision, allocatable :: dnus1(:),times1(:,:),errs1(:),dsizes1(:)
double precision, allocatable :: dnus2(:),times2(:,:),errs2(:),dsizes2(:)
double precision, allocatable :: dnus3(:),times3(:,:),errs3(:),dsizes3(:)
double precision, allocatable :: dnus4(:),times4(:,:),errs4(:),dsizes4(:),dkranks(:)
integer, allocatable          :: kranks1(:)
integer, allocatable          :: kranks2(:)
integer, allocatable          :: kranks3(:)
integer, allocatable          :: kranks4(:)

allocate(dnus1(i1:i2),times1(5,i1:i2),errs1(i1:i2),dsizes1(i1:i2),kranks1(i1:i2))
allocate(dnus2(i1:i2),times2(5,i1:i2),errs2(i1:i2),dsizes2(i1:i2),kranks2(i1:i2))
allocate(dnus3(i1:i2),times3(5,i1:i2),errs3(i1:i2),dsizes3(i1:i2),kranks3(i1:i2))
allocate(dnus4(i1:i2),times4(5,i1:i2),errs4(i1:i2),dsizes4(i1:i2),kranks4(i1:i2))


da      =-0.25d0
db      = 0.00d0

decay   = 0
call comp_err(decay,i1,i2,da,db,times1,dnus1,errs1,dsizes1,kranks1)

decay   = 0.5d0
call comp_err(decay,i1,i2,da,db,times2,dnus2,errs2,dsizes2,kranks2)

decay   = 1.0d0
call comp_err(decay,i1,i2,da,db,times3,dnus3,errs3,dsizes3,kranks3)

decay   = 2.0d0
call comp_err(decay,i1,i2,da,db,times4,dnus4,errs4,dsizes4,kranks4)

! build the graph

iw2 = 21
open(iw2,FILE='graph_transerr.py')
call pyplot_begin(iw2,istatus)
m = i2-i1+1

call pyplot_add_function(istatus,1,"$\mathcal{O}\left(1\right)$",m,dnus1,errs1)
call pyplot_add_function(istatus,2,"$\mathcal{O}\left(n^{-1/2}\right)$",m,dnus2,errs2)
call pyplot_add_function(istatus,3,"$\mathcal{O}\left(n^{-1}\right)$",m,dnus3,errs3)
call pyplot_add_function(istatus,4,"$\mathcal{O}\left(n^{-2}\right)$",m,dnus4,errs4)

call pyplot_xlogscale(istatus)
call pyplot_ylogscale(istatus)
call pyplot_xlabel(istatus,"N")
call pyplot_ylabel(istatus,"maximum absolute error")
call pyplot_end(istatus,"graph_transerr.pdf")
close(iw2)



end subroutine


subroutine comp_err(decay,i1,i2,da,db,times,dnus,errs,dsizes,kranks)
use utils
use chebyshev
use idecomp
use jacobi_exp
use jacobi_transform
implicit double precision (a-h,o-z)


double precision  :: dnus(i1:i2),times(5,i1:i2),errs(i1:i2),dsizes(i1:i2)
integer           :: kranks(i1:i2)

double precision, allocatable :: vals(:,:),ts(:),whts(:)
type(jacobi_expansion_data)   :: expdata
type(jacobi_transform_data)   :: jacdata
double precision, allocatable :: x(:),y(:),x0(:),y0(:),xx(:),yy(:),xx2(:)

pi       = acos(-1.0d0)
ima      = (0.0d0,1.0d0)
ntimes   = 1


do i=i1,i2

eps      = 1.0d-11
n        = 2**i
iffactor = 1
ifeval   = 0

if (n .lt. 100)   n = 100
if (n .gt. 10**8) n = 10**8

dmax     = n
dnu      = n
dnus(i)  = dnu


call prini("n = ",n)
call elapsed(t1)
call jacobi_expansion(eps,iffactor,dmax,da,db,expdata)
call jacobi_transform_prepare(expdata,n,jacdata)
call elapsed(t2)


times(1,i) = t2-t1
kranks(i)  = expdata%krank

call jacobi_expansion_size(expdata,dsize1)
call jacobi_transform_size(jacdata,dsize2)
dsize = dsize1+dsize2
call prin2("jacobi data size (MB) = ",dsize)
dsizes(i) = dsize

call prini("krank = ",expdata%krank)
call prin2("prepare time = ",t2-t1)


! construct an input vector and allocate memory for the output
allocate(x(n),y0(n),y(n),x0(n))


do j=1,n
call random_number(dd1)
call random_number(dd2)
dd = j
x(j) = sqrt(-2*log(dd1))*cos(2*pi*dd2)/dd**decay
end do



call elapsed(t1)
call jacobi_transform_forward(jacdata,x,y)
call elapsed(t2)
dtime = (t2-t1)
call prin2("forward apply time = ",dtime)

times(2,i) = dtime

call elapsed(t1)
call jacobi_transform_backward(jacdata,y,x0)
call elapsed(t2)
call prin2("backward  apply time = ",t2-t1)
times(3,i) = t2-t1


! call jacobi_transform_backward_bf(da,db,n,n,jacdata%ts,jacdata%whts,y,x0)

times(4,i) = n*log(n+0.0d0)



call prin2("composition error =",maxval(abs(x-x0)))
errs(i) = maxval(abs(x-x0))


deallocate(x,y,x0,y0)
call jacobi_transform_destroy(jacdata)
end do



end subroutine



subroutine comparison_slevinsky()
use utils
use chebyshev
use idecomp
use jacobi_exp
use jacobi_transform
implicit double precision (a-h,o-z)

type(jacobi_expansion_data)   :: expdata
type(jacobi_transform_data)   :: jacdata
double precision, allocatable :: x(:),y(:),x0(:),y0(:),xx(:),yy(:),xx2(:)

double precision, allocatable :: dnus(:),times(:,:),errs(:),dsizes(:)
integer, allocatable          :: kranks(:)

pi       = acos(-1.0d0)
ima      = (0.0d0,1.0d0)
ntimes   = 1



!
!  Do comparisons with Slevinsky's algorithm
!


iw = 20
open(iw,FILE='transform.dat')

read(iw,*) da
read(iw,*) db
read(iw,*) m

print *,da,db,m

allocate(times(5,m),errs(m),dnus(m),kranks(m))

do i=1,m

read (iw,*) dnus(i)
read (iw,*) times(4,i)
read (iw,*) times(5,i)


eps      = 1.0d-12
dnu      = dnus(i)
n        = dnu
dmax     = n
iffactor = 1

call prini("n = ",n)

! prepare
call elapsed(t1)
call jacobi_expansion(eps,iffactor,dmax,da,db,expdata)
call jacobi_transform_prepare(expdata,n,jacdata)
call elapsed(t2)
times(1,i) = t2-t1
kranks(i)  = expdata%krank


! construct an input vector and allocate memory for the output
allocate(x(n),y(n),x0(n))

do j=1,n
call random_number(dd1)
call random_number(dd2)
dd = j
x(j) = sqrt(-2*log(dd1))*cos(2*pi*dd2)/dd**2
end do

call elapsed(t1)
do j=1,ntimes
call jacobi_transform_forward(jacdata,x,y)
end do
call elapsed(t2)
dtime = (t2-t1)/ntimes
call prin2("forward apply time = ",dtime)
times(2,i) = dtime

call elapsed(t1)
do j=1,ntimes
call jacobi_transform_backward(jacdata,y,x0)
end do
call elapsed(t2)
dtime = (t2-t1)/ntimes
call prin2("backward  apply time = ",dtime)
times(3,i) = dtime

errs(i) = maxval(abs(x-x0))
call prin2("composition error =",errs(i))
deallocate(x,y,x0)

end do

! write out the graphs



! produce the python scripts which constructs the comparison plot

iw2 = 21
open(iw2,FILE='graph_transform1.py')
call pyplot_begin(iw2,istatus)

call pyplot_add_function(istatus,1,"Forward Jacobi transform",m,dnus,times(2,:))
call pyplot_add_function(istatus,3,"Inverse Jacobi transform",m,dnus,times(3,:))
call pyplot_add_function(istatus,7,"Slevinsky Jacobi-to-Chebyshev",m,dnus,times(4,:))
call pyplot_add_function(istatus,8,"Slevinsky Chebyshev-to-Jacobi",m,dnus,times(5,:))

call pyplot_xlogscale(istatus)
call pyplot_ylogscale(istatus)
call pyplot_xlabel(istatus,"n")
call pyplot_ylabel(istatus,"execution time (s)")
call pyplot_end(istatus,"graph_transform1.pdf")
close(iw2)


iw2 = 21
open(iw2,FILE='graph_transform2.py')
call pyplot_begin(iw2,istatus)

call pyplot_add_function(istatus,1,"Precomputation time",m,dnus,times(1,:))
call pyplot_add_function(istatus,6,"Slevinsky Jacobi-to-Chebyshev",m,dnus,times(4,:))
call pyplot_add_function(istatus,7,"Slevinsky Chebyshev-to-Jacobi",m,dnus,times(5,:))

call pyplot_xlogscale(istatus)
call pyplot_ylogscale(istatus)
call pyplot_xlabel(istatus,"n")
call pyplot_ylabel(istatus,"execution time (s)")
call pyplot_end(istatus,"graph_transform2.pdf")
close(iw2)

!
!  Run the second set of experiments and construct the first table
!


read(iw,*) da
read(iw,*) db
read(iw,*) m

print *,da,db,m

do i=1,m

read (iw,*) dnus(i)
read (iw,*) times(4,i)
read (iw,*) times(5,i)


eps      = 1.0d-12
dnu      = dnus(i)
n        = dnu
dmax     = n
iffactor = 1

call prini("n = ",n)

! prepare
call elapsed(t1)
call jacobi_expansion(eps,iffactor,dmax,da,db,expdata)
call jacobi_transform_prepare(expdata,n,jacdata)
call elapsed(t2)
times(6,i) = t2-t1
kranks(i)  = expdata%krank


! construct an input vector and allocate memory for the output
allocate(x(n),y(n),x0(n))

do j=1,n
call random_number(dd1)
call random_number(dd2)
dd = j
x(j) = sqrt(-2*log(dd1))*cos(2*pi*dd2)/dd**2
end do

call elapsed(t1)
do j=1,ntimes
call jacobi_transform_forward(jacdata,x,y)
end do
call elapsed(t2)
dtime = (t2-t1)/ntimes
call prin2("forward apply time = ",dtime)
times(2,i) = dtime

call elapsed(t1)
do j=1,ntimes
call jacobi_transform_backward(jacdata,y,x0)
end do
call elapsed(t2)
dtime = (t2-t1)/ntimes
call prin2("backward  apply time = ",dtime)
times(3,i) = dtime

errs(i) = maxval(abs(x-x0))
call prin2("composition error =",errs(i))
deallocate(x,y,x0)

end do

close(iw)



!!!!!!!!!!! a LaTeX table


iw2 = 21
open(iw2,FILE='tabletransform.tex')

write(iw2,*) "\begin{tabular}{lccc}"
write(iw2,*) "\toprule"
write(iw2,*) "$n$   & Forward Jacobi   & Chebyshev-Jacobi             & Ratio  \\"
write(iw2,*) "      & transform time    & transform time &  \\"
write(iw2,*) "      & algorithm of Section~\ref{section:transform} & algorithm of \cite{Slevinsky1} &   \\"
write(iw2,*) "\midrule"

do i=1,m
n = dnus(i)
call write_table_integer_sep(iw2,n)
call write_table_next(iw2)
call write_table_double(iw2,times(2,i))
call write_table_next(iw2)
call write_table_double(iw2,times(4,i))
call write_table_next(iw2)
call write_table_double(iw2,times(4,i)/times(2,i))
call write_table_nextline(iw2)
end do

write (iw2,*) "\bottomrule"
write (iw2,*) "\end{tabular}"

close(iw2)


end subroutine

