!
!  Conduct the experiments in Section~7.2 of the preprint
!
!    James Bremer and Haizhao Yang,  "Fast algorithms for Jacobi polynomials 
!    via nonoscillatory phase functions."


program jacobi_experiment2

use utils
use chebyshev

implicit double precision (a-h,o-z)


double precision, allocatable :: xs(:),whts(:),whts0(:)
integer, allocatable          :: idxs0(:)
double precision, allocatable :: dnus(:),times(:,:),errs(:)


pi       = acos(-1.0d0)
ima      = (0.0d0,1.0d0)
ntimes   = 1



! read data from townsend.dat (generated via Julia code)
iw = 20
open(iw,FILE='quad.dat')


read(iw,*) da
read(iw,*) db
read(iw,*) m


allocate(times(6,m),errs(m),dnus(m))
allocate(whts0(100),idxs0(100))

do i=1,m

read (iw,*) dnus(i)
read (iw,*) times(2,i)


dnu      = dnus(i)
n        = dnu


allocate(xs(n),whts(n))
call elapsed(t1)
do j=1,ntimes
call jacobi_quad(n,da,db,xs,whts)
end do
call elapsed(t2)
dtime      = (t2-t1)/ntimes

times(1,i) = dtime



do j=1,100
read(iw,*) idxs0(j)
read(iw,*) whts0(j)

wht  = whts(idxs0(j))
wht0 = whts0(j)
dd  = abs((wht-wht0)/wht0)
end do

derr = maxval(abs(whts0-whts(idxs0))/abs(whts0))


call prini("n = ",n)
call prin2("quad time = ",dtime)
call prin2("max relative error = ",derr)


deallocate(xs,whts)

end do

call prina("")
call prina("")
call prina("")
call prina("")

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

read(iw,*) da
read(iw,*) db
read(iw,*) m


do i=1,m

read (iw,*) dnus(i)
read (iw,*) times(4,i)


dnu      = dnus(i)
n        = dnu

allocate(xs(n),whts(n))

call elapsed(t1)
do j=1,ntimes
call jacobi_quad(n,da,db,xs,whts)
end do
call elapsed(t2)
dtime      = (t2-t1)/ntimes


times(3,i) = dtime


do j=1,100
read(iw,*) idxs0(j)
read(iw,*) whts0(j)

wht  = whts(idxs0(j))
wht0 = whts0(j)
dd  = abs((wht-wht0)/wht0)

end do


derr = maxval(abs(whts0-whts(idxs0))/abs(whts0))


call prini("n = ",n)
call prin2("quad time = ",dtime)
call prin2("max relative error = ",derr)

deallocate(xs,whts)

end do




! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

read(iw,*) da
read(iw,*) db
read(iw,*) m


do i=1,m

read (iw,*) dnus(i)
read (iw,*) times(6,i)


dnu      = dnus(i)
n        = dnu

allocate(xs(n),whts(n))

call elapsed(t1)
do j=1,ntimes
call jacobi_quad(n,da,db,xs,whts)
end do
call elapsed(t2)
dtime      = (t2-t1)/ntimes


times(5,i) = dtime


do j=1,100
read(iw,*) idxs0(j)
read(iw,*) whts0(j)

wht  = whts(idxs0(j))
wht0 = whts0(j)
dd  = abs((wht-wht0)/wht0)

end do


derr = maxval(abs(whts0-whts(idxs0))/abs(whts0))
errs(i)=derr


call prini("n = ",n)
call prin2("quad time = ",dtime)
call prin2("max relative error = ",derr)


deallocate(xs,whts)

end do

close(iw)

! produce the python scripts which construct the comparison plots

iw = 20
open(iw,FILE='graph_quad1.py')
call pyplot_begin(iw,istatus)

call pyplot_add_function(istatus,1,"Algorithm of this paper",m,dnus,times(1,:))
call pyplot_add_function(istatus,7,"Hale-Townsend algorithm",m,dnus,times(2,:))

call pyplot_xlogscale(istatus)
call pyplot_ylogscale(istatus)
call pyplot_xlabel(istatus,"n")
call pyplot_ylabel(istatus,"execution time (s)")
call pyplot_end(istatus,"graph_quad1.pdf")
close(iw)



iw = 20
open(iw,FILE='graph_quad2.py')
call pyplot_begin(iw,istatus)

call pyplot_add_function(istatus,1,"Algorithm of this paper",m,dnus,times(3,:))
call pyplot_add_function(istatus,7,"Hale-Townsend algorithm",m,dnus,times(4,:))

call pyplot_xlogscale(istatus)
call pyplot_ylogscale(istatus)
call pyplot_xlabel(istatus,"n")
call pyplot_ylabel(istatus,"execution time (s)")
call pyplot_end(istatus,"graph_quad2.pdf")
close(iw)

!!!!!!!!!!! a LaTeX table


iw2 = 21
open(iw2,FILE='tablequad.tex')

write(iw2,*) "\begin{tabular}{lcccc}"
write(iw2,*) "\toprule"
write(iw2,*) "$n$   & Running time of the & Running time of  & Ratio & Maximum relative\\"
write(iw2,*) "      & algorithm of Section~\ref{section:quad} & the algorithm of  \cite{Hale-Townsend} && error in weights  \\"
write(iw2,*) "\midrule"

do i=1,m
n = dnus(i)
call write_table_integer_sep(iw2,n)
call write_table_next(iw2)
call write_table_double(iw2,times(5,i))
call write_table_next(iw2)
call write_table_double(iw2,times(6,i))
call write_table_next(iw2)
call write_table_double(iw2,times(6,i)/times(5,i))
call write_table_next(iw2)
call write_table_double(iw2,errs(i))
call write_table_nextline(iw2)
end do

write (iw2,*) "\bottomrule"
write (iw2,*) "\end{tabular}"

close(iw2)

end program



