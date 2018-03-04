program test_jacobi_transform

use utils
use chebyshev
use idecomp
use jacobi_exp
use jacobi_transform

implicit double precision (a-h,o-z)

double precision, allocatable :: vals(:,:),ts(:),whts(:)

type(jacobi_expansion_data)   :: expdata
type(jacobi_transform_data)   :: jacdata
double precision, allocatable :: x(:),y(:),x0(:),y0(:),xx(:),yy(:),xx2(:)

double precision, allocatable :: dnus(:),times(:,:),errs(:),dsizes(:)
integer, allocatable          :: kranks(:)

pi       = acos(-1.0d0)
ima      = (0.0d0,1.0d0)
ntimes   = 1
da       = 0.25d0
db       = 0.33d0

i1       = 4
i2       = 18

allocate(times(5,i1:i2),errs(i1:i2),dsizes(i1:i2),kranks(i1:i2),xx(i1:i2))
times   = 0 
errs    = 0
dsizes  = 0
kranks  = 0 

do i=i1,i2

eps      = 1.0d-12
n        = int(2d0**i)
dmax     = n
iffactor = 1
ifeval   = 0
xx(i)    = n


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
x(j) = sqrt(-2*log(dd1))*cos(2*pi*dd2)/dd**2
end do


call elapsed(t1)
call jacobi_transform_forward(jacdata,x,y)
call elapsed(t2)

dtime = (t2-t1)
call prin2("forward apply time = ",dtime)
times(2,i) = dtime

! call jacobi_transform_forward_bf(da,db,n,n,jacdata%ts,jacdata%whts,x,y0)
! print *,norm2(y-y0)


call elapsed(t1)
call jacobi_transform_backward(jacdata,y,x0)
call elapsed(t2)
call prin2("backward  apply time = ",t2-t1)
times(3,i) = t2-t1


! call jacobi_transform_backward_bf(da,db,n,n,jacdata%ts,jacdata%whts,y,x)
! print *,norm2(x-x0)



times(4,i) = n*log(n+0.0d0)

call prin2("composition error =",maxval(abs(x-x0)))
errs(i) = maxval(abs(x-x0))

deallocate(x,y,x0,y0)
call jacobi_transform_destroy(jacdata)
end do


times(4,:) = times(4,:)

print *,""
print *,""
print *,"n     krank    prepare time   forward time    backward time     error   "
print *,"---------------------------------------------------------------------------"

write (13,*) ""
write (13,*) ""
write(13,*) "n     krank    prepare time   forward time    backward time     error   "
write(13,*) "---------------------------------------------------------------------------"

do i=i1,i2
write (*,"(1X,I2.2,5X,I3.3,5X,D8.3,8X,D8.3,8X,D8.3,8X,D8.3)")  &
  i,kranks(i),times(1,i),times(2,i),times(3,i),errs(i)

! write (13,"(1X,I2.2,5X,I3.3,5X,D8.3,8X,D8.3,8X,D8.3,8X,D8.3)")  &
!   i,kranks(i),times(1,i),times(2,i),times(3,i),errs(i)

end do


! python script

iw = 20
k  = i2-i1+1
open(iw,FILE='transform1.py')
call pyplot_begin(iw,istatus)

!call pyplot_add_function(istatus,1,"prepare time",k,xx,times(1,:))
call pyplot_add_function(istatus,1,"forward transform",k,xx,times(2,:))
call pyplot_add_function(istatus,2,"backward transform",k,xx,times(3,:))

! call pyplot_add_function(istatus,3,"backward transform",k,xx,times(3,:))
! call pyplot_add_function(istatus,4,"backward transform",k,xx,times(3,:))

call pyplot_xlogscale(istatus)
call pyplot_ylogscale(istatus)
call pyplot_xlabel(istatus,"N")
call pyplot_ylabel(istatus,"execution time (s)")
call pyplot_end(istatus,"transform1.png")

!call pyplot_function3("jacobi_transform1.py","transform1.png","","",i2-i1+1,xx,&
!   "forward apply time",times(2,:), &
!   "backward apply time",times(3,:),&
!   "n log(n)",times(4,:))

end program



