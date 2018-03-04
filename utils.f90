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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  This module contains miscellaneous utility routines which can be divided into the 
!  following categories:
!
!  Printing routines:
!  -----------------
!
!  The folllowing routines output the contents of arrays.  They have two principal
!  advantages over simply using "print": (1) the output of these routines is also
!  written into the file fort.13 for later consideration, (2) the output is formatted
!  in a consistent fashion.
!
!    prin2 - output an array of doubles with 7 digits of each element displayed 
!    prind - output an array of doubles with 15 digits of each element displayed1
!    prini - output an array of integers
!    prinl - output an array of long integers
!    prina - output a string
!
!  Plotting:
!  ------------------
!
!  This module contains the following simple plotting routines:
!
!    pyplot_function - this is a quick utility routine which produces a python
!      script that generates a plot of a function
!
!
!  Table generatation:
!  ------------------
!
!   This module contains the following extremely primitive routines for generating
!
!
!  Other routines:
!  ---------------
!
!    elapsed - return the wall clock time in seconds which has elapsed since some
!     arbitrary point in the past 
!    insort - sort an array of real numbers
!    insorti  - sort an array of integers
!    quicksort - sort an array of real numbers
!    quicksorti - sort an array of integers
!    qrsolv - solve a system of linear equations via a QR decomposition
!    eye - return a (k,k) identity matrix
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


module utils

interface prin2
   module procedure prin2_0
   module procedure prin2_1
   module procedure prin2_2
   module procedure prin2_3
end interface prin2

interface prind
   module procedure prind_0
   module procedure prind_1
   module procedure prind_2
end interface prind

interface prini
   module procedure prini_0
   module procedure prini_1
   module procedure prini_2
end interface prini

interface prinl
   module procedure prinl_0
   module procedure prinl_1
   module procedure prinl_2
end interface prinl

contains

subroutine prin2_0(str,a)
implicit double precision (a-h,o-z)

double precision a
character(len=*), intent(in) :: str

print *,str
print "(8(2x,e15.7))",a

write (13,*) str
write (13,"(8(2x,e15.7))") a

end subroutine

subroutine prin2_1(str,a)
implicit double precision (a-h,o-z)

double precision, intent (in) :: a(:)
character(len=*), intent(in) :: str

print *,str
print "(8(2x,e15.7))",a

write (13,*) str
write (13,"(8(2x,e15.7))") a

end subroutine

subroutine prin2_2(str,a)
implicit double precision (a-h,o-z)

double precision, intent (in) :: a(:,:)
character(len=*), intent(in) :: str

print *,str
print "(8(2x,e15.7))",a

write (13,*) str
write (13,"(8(2x,e15.7))") a

end subroutine


subroutine prin2_3(str,a)
implicit double precision (a-h,o-z)

double precision, intent (in) :: a(:,:,:)
character(len=*), intent(in) :: str

print *,str
print "(8(2x,e15.7))",a

write (13,*) str
write (13,"(8(2x,e15.7))") a

end subroutine

subroutine prind_0(str,a)
implicit double precision (a-h,o-z)

double precision :: a
character(len=*), intent(in) :: str

print *,str
print "(5(2x,e24.16))",a

write (13,*) str
write (13,"(5(2x,e24.16))") a

end subroutine

subroutine prind_1(str,a)
implicit double precision (a-h,o-z)

double precision,intent(in) :: a(:)
character(len=*), intent(in) :: str

print *,str
print "(5(2x,e24.16))",a

write (13,*) str
write (13,"(5(2x,e24.16))") a

end subroutine


subroutine prind_2(str,a)
implicit double precision (a-h,o-z)

double precision,intent(in) :: a(:,:)
character(len=*), intent(in) :: str

print *,str
print "(5(2x,e24.16))",a

write (13,*) str
write (13,"(5(2x,e24.16))") a

end subroutine



subroutine prini_0(str,a)
implicit double precision (a-h,o-z)

integer,intent(in) :: a
character(len=*), intent(in) :: str

print *,str
print "(8(2x,I9))",a

write (13,*) str
write (13,"(8(2x,I9))") a

end subroutine

subroutine prini_1(str,a)
implicit double precision (a-h,o-z)

integer,intent(in) :: a(:)
character(len=*), intent(in) :: str

print *,str
print "(8(2x,I8))",a

write (13,*) str
write (13,"(8(2x,I8))") a

end subroutine



subroutine prini_2(str,a)
implicit double precision (a-h,o-z)

integer,intent(in) :: a(:,:)
character(len=*), intent(in) :: str

print *,str
print "(8(2x,I8))",a

write (13,*) str
write (13,"(8(2x,I8))") a

end subroutine

subroutine prina(str)
implicit double precision (a-h,o-z)

character(len=*), intent(in) :: str

print *,str
write (13,*) str

end subroutine


subroutine prinl_0(str,a)
implicit double precision (a-h,o-z)

integer,intent(in) :: a
character(len=*), intent(in) :: str

print *,str
print "(6(2x,I13))",a

write (13,*) str
write (13,"(6(2x,I13))") a

end subroutine

subroutine prinl_1(str,a)
implicit double precision (a-h,o-z)

integer,intent(in) :: a(:)
character(len=*), intent(in) :: str

print *,str
print "(6(2x,I13))",a

write (13,*) str
write (13,"(6(2x,I13))") a

end subroutine

subroutine prinl_2(str,a)
implicit double precision (a-h,o-z)

integer,intent(in) :: a(:,:)
character(len=*), intent(in) :: str

print *,str
print "(6(2x,I13))",a

write (13,*) str
write (13,"(6(2x,I13))") a

end subroutine



subroutine elapsed(t)
implicit double precision (a-h,o-z)
integer*8 i,irate
real t1
call system_clock(i,irate)

dd = i
dd = dd/irate
t = dd
return
end subroutine




subroutine insort(k,a)
implicit double precision (a-h,o-z)
integer, intent(in)              :: k
double precision, intent (inout) :: a(k)

if (k .le. 1) return

do i=2,k
val=a(i)
j=i-1
do while (j .ge. 1 .AND. a(j) .gt. val) 
a(j+1)=a(j)
j=j-1
end do
a(j+1)=val
end do
end subroutine


subroutine insorti(k,ia)
implicit double precision (a-h,o-z)
integer, intent(in)              :: k
integer, intent (inout)          :: ia(k)

if (k .le. 1) return

do i=2,k
ival=ia(i)
j=i-1
do while (j .ge. 1 .AND. ia(j) .gt. ival) 
ia(j+1)=ia(j)
j=j-1
end do
ia(j+1)=ival
end do
end subroutine


subroutine insorti2(k,ia,idxs)
implicit double precision (a-h,o-z)
integer, intent(in)              :: k
integer, intent (inout)          :: ia(k),idxs(k)

if (k .le. 1) return

do i=2,k
ival   = ia(i)
idxval = idxs(i)
j=i-1
do while (j .ge. 1 .AND. ia(j) .gt. ival) 
ia(j+1)   = ia(j)
idxs(j+1) = idxs(j)
j=j-1
end do
ia(j+1)   = ival
idxs(j+1) = idxval
end do
end subroutine


subroutine quicksort(n,vals)
implicit double precision (a-h,o-z)
dimension istack(2,20000)
dimension vals(1),idxs(1)
!
!       Sort a list of double precision numbers.
!
        if (n .lt. 100) then
        call insort(n,vals)
        return
        endif
!
        maxstack = 10000
        k        = 60
!
        m = 1
        istack(1,1) = 1
        istack(2,1) = n
!
 1000 continue
        if (m .eq. 0) goto 1100
        i1 = istack(1,m)
        i2 = istack(2,m)
        m=m-1
!
        l = i2-i1+1
        if (l .le. k) then
        call insort(l,vals(i1))
        goto 1000
        endif
!
!       Otherwise perform quicksort.
!
        call quicksort01(vals,i1,i2,i3)
!
!       This should never happen, but just in case ...
!
        if (m+2 .ge. maxstack) then
        print *,"quicksort out of memory"
        stop
        endif
!
!       Make sure the smaller half is processed first to reduce storage
!       to O(logn).
!             
        n1 = i3-i1+1
        n2 = i2-i3
!
        if (n2 .lt. n1) then
!
        m = m+1
        istack(1,m) = i1
        istack(2,m) = i3
!
        m = m+1
        istack(1,m) = i3+1
        istack(2,m) = i2
!
        else
!
        m = m+1
        istack(1,m) = i3+1
        istack(2,m) = i2
!
        m = m+1
        istack(1,m) = i1
        istack(2,m) = i3
!
        endif
!
        goto 1000
 1100 continue 
end subroutine


        subroutine quicksort01(vals,i1,i2,i3)
        implicit double precision (a-h,o-z)
        dimension vals(1)
!
!       Randomly choose a pivot index.
!
!        call corrand3(1,r)
        call random_number(r)
        ipiv = i1+floor((i2-i1)*r)
!
!        ipiv = i1+(i2-i1)/2
!
        val  = vals(ipiv)
!
!       Swap the pivot element and the last element.
!
        vals(ipiv) = vals(i2)
        vals(i2)   = val
!
       i3 = i1
!
        do 1000 i=i1,i2-1
!
        if( vals(i) .lt. val) then
        d  = vals(i)
!
        vals(i)  = vals(i3)
        vals(i3) = d       
!
        i3=i3+1
        endif
!
 1000 continue
!
        dd = vals(i3)
        vals(i3) = vals(i2)
        vals(i2) = dd
!
        end subroutine


        subroutine quicksorti(n,ivals)
        implicit double precision (a-h,o-z)
        dimension istack(2,20000),ivals(1)
!
!       Sort a list of integers.
!
        if (n .lt. 60) then
        call insorti(n,ivals)
        return
        endif
!
        maxstack = 10000
        k        = 60
!
        nstack      = 1
        istack(1,1) = 1
        istack(2,1) = n
!
 1000 continue
        if (nstack .eq. 0) goto 1100
        i1 = istack(1,nstack)
        i2 = istack(2,nstack)
        nstack=nstack-1
!
!
        l = i2-i1+1
        if (l .le. k) then
        call insorti(l,ivals(i1))
        goto 1000
        endif
!
!       Otherwise perform quicksort.
!
        call quicksorti0(ivals,i1,i2,i3)
!
!       This should never happen, but just in case ...
!
        if (nstack+2 .ge. maxstack) then
        print *,"quicksort out of memory"
        stop
        endif
!
!       Make sure the smaller half is processed first to reduce storage
!       to O(logn).
!             
        n1 = i3-i1+1
        n2 = i2-(i3+1)+1
!
        if (n2 .lt. n1) then
!
        nstack = nstack+1
        istack(1,nstack) = i1
        istack(2,nstack) = i3
!
        nstack = nstack+1
        istack(1,nstack) = i3+1
        istack(2,nstack) = i2
!
        else
!
        nstack=nstack+1
        istack(1,nstack) = i3+1
        istack(2,nstack) = i2
!
        nstack=nstack+1
        istack(1,nstack) = i1
        istack(2,nstack) = i3
!
        endif
!
        goto 1000
 1100 continue
        end subroutine


        subroutine quicksorti0(ivals,i1,i2,i3)
        implicit double precision (a-h,o-z)
        dimension ivals(1)
!
!       Randomly choose a pivot index.
!
!        call corrand3(1,r)
        call random_number(r)
        ipiv = i1+floor((i2-i1)*r)
!
!        ipiv = i1+(i2-i1)/2
!
        ival  = ivals(ipiv)
!
!       Swap the pivot element and the last element.
!
        ivals(ipiv) = ivals(i2)
        ivals(i2)   = ival
!
        i3 = i1
!
        do 1000 i=i1,i2-1
        if( ivals(i) .lt. ival) then
        id  = ivals(i)
!
        ivals(i)  = ivals(i3)
        ivals(i3) = id
!
        i3=i3+1
        endif
 1000 continue
!
        id        = ivals(i3)
        ivals(i3) = ivals(i2)
        ivals(i2) = id
!
        end subroutine


subroutine qrsolv(a,n,rhs)
implicit double precision (a-h,o-z)

integer            :: n
double precision   :: a(n,n),rhs(n)

!
!  This subroutine uses a version of QR-decomposition to solve the equation
!  A x = b.  Both the input matrix a and the right-hand side are destroyed
!  b ythe routine.
!
!  Input parameters:
!    a - the (n,n) matrix of coefficients
!    n - an integer specifying the size of the system of 
!    rhs - a vector of length n speciying the rhs of the system
!
!  Output parameters:
!
!   rhs - upon return, the solution of the linear system


double precision :: aa(2),u(2,2)

! 
! transpose the input matrix a 
!

size22=0
do i=1,n
do j=1,i
d=a(j,i)
a(j,i)=a(i,j)
a(i,j)=d
size22=size22+a(j,i)**2
size22=size22+a(i,j)**2
end do
end do

!
!  Reduce to upper triangular 
!
do i=1,n-1
do j=n,i+1,-1
aa(1)=a(i,j-1)
aa(2)=a(i,j)

u22=-aa(1)
u12=aa(2)
d=u22**2+u12**2
if(d .lt. size22*1.0d-66) then
u(2,2)=1
u(1,2)=0
u(1,1)=1
u(2,1)=0
else
d=sqrt(d)
u(2,2)=u22/d
u(1,2)=u12/d
u(1,1)=-u(2,2)
u(2,1)=u(1,2)
endif

do ii=i,n
d1=u(1,1)*a(ii,j-1)+u(1,2)*a(ii,j)
d2=u(2,1)*a(ii,j-1)+u(2,2)*a(ii,j) 
a(ii,j-1)=d1
a(ii,j)=d2
end do

d1=u(1,1)*rhs(j-1)+u(1,2)*rhs(j)
d2=u(2,1)*rhs(j-1)+u(2,2)*rhs(j)
rhs(j-1)=d1
rhs(j)=d2
end do
end do

!
!  Apply the inverse of the triangular matrix
! 

rhs(n)=rhs(n)/a(n,n)
do i=n-1,1,-1
d=0
do j=n,i+1,-1
d=d+a(j,i)*rhs(j)
end do
rhs(i)=(rhs(i)-d)/a(i,i)
end do

return
end subroutine


function eye(n) result(a)
implicit double precision (a-h,o-z)

double precision, allocatable :: a(:,:)
integer n

!
!  Return an identity matrix which is dimensioned (n,n).
!

allocate(a(n,n))
a = 0
do i=1,n
a(i,i)=1.0d0
end do

end function



subroutine pyplot_begin(iw,istatus)
implicit double precision (a-h,o-z)

integer           :: iw,istatus

!
!  Begin the process of constructing a python script for producing a plot
!  of a function or collection of functions.
!  
!  Input parameters:
!    iw - the fortran unit number of the file to write output to; this must
!      be <= 255
!
!  Output parameters:
!    istatus - a status word used to keep track of certain parameters
!

write(iw,"(A)") "import numpy as np"
write(iw,"(A)") "import matplotlib.pyplot as plt"
write(iw,"(A)") "fig, ax = plt.subplots()"


write(iw,"(A)") "from matplotlib import rc"
write(iw,"(A)") "rc('font',**{'family':'sans-serif','sans-serif':['Helvetica'],'size':16})"
write(iw,"(A)") "rc('text', usetex=True)"


istatus = iw


end subroutine



subroutine pyplot_xlogscale(istatus)
implicit double precision (a-h,o-z)

integer               :: istatus
integer, parameter    :: mask1 = Z'000000FF'
integer, parameter    :: mask2 = Z'0000FF00'
integer, parameter    :: mask3 = Z'00010000'
integer, parameter    :: mask4 = Z'00020000'
integer, parameter    :: mask5 = Z'00040000'


iw      = iand(istatus,mask1)
idx     = ishft(iand(istatus,mask2),-8)
ilabel  = ishft(iand(istatus,mask3),-16)
ilogx   = ishft(iand(istatus,mask4),-17)
ilogy   = ishft(iand(istatus,mask5),-18)

ilogx = 1

write (iw,"(A)") "ax.set_xscale('log')"
istatus = iw + ishft(idx,8) + ishft(ilabel,16) + ishft(ilogx,17) + ishft(ilogy,18)

end subroutine


subroutine pyplot_ylogscale(istatus)
implicit double precision (a-h,o-z)

integer               :: istatus
integer, parameter    :: mask1 = Z'000000FF'
integer, parameter    :: mask2 = Z'0000FF00'
integer, parameter    :: mask3 = Z'00010000'
integer, parameter    :: mask4 = Z'00020000'
integer, parameter    :: mask5 = Z'00040000'


iw      = iand(istatus,mask1)
idx     = ishft(iand(istatus,mask2),-8)
ilabel  = ishft(iand(istatus,mask3),-16)
ilogx   = ishft(iand(istatus,mask4),-17)
ilogy   = ishft(iand(istatus,mask5),-18)

ilogy = 1

write (iw,"(A)") "ax.set_yscale('log')"
istatus = iw + ishft(idx,8) + ishft(ilabel,16) + ishft(ilogx,17) + ishft(ilogy,18)

end subroutine


subroutine pyplot_xlabel(istatus,xlabel)
implicit double precision (a-h,o-z)

!
!
!

integer               :: istatus
character(len=*)      :: xlabel

integer, parameter    :: mask1 = Z'000000FF'
integer, parameter    :: mask2 = Z'0000FF00'
integer, parameter    :: mask3 = Z'00010000'
integer, parameter    :: mask4 = Z'00020000'
integer, parameter    :: mask5 = Z'00040000'


iw      = iand(istatus,mask1)
idx     = ishft(iand(istatus,mask2),-8)
ilabel  = ishft(iand(istatus,mask3),-16)
ilogx   = ishft(iand(istatus,mask4),-17)
ilogy   = ishft(iand(istatus,mask5),-18)

write (iw,"(A,A,A)") 'ax.set(xlabel=r"',xlabel,'")'

istatus = iw + ishft(idx,8) + ishft(ilabel,16) + ishft(ilogx,17) + ishft(ilogy,18)

end subroutine


subroutine pyplot_ylabel(istatus,ylabel)
implicit double precision (a-h,o-z)

!
!
!

integer               :: istatus
character(len=*)      :: ylabel

integer, parameter    :: mask1 = Z'000000FF'
integer, parameter    :: mask2 = Z'0000FF00'
integer, parameter    :: mask3 = Z'00010000'
integer, parameter    :: mask4 = Z'00020000'
integer, parameter    :: mask5 = Z'00040000'


iw      = iand(istatus,mask1)
idx     = ishft(iand(istatus,mask2),-8)
ilabel  = ishft(iand(istatus,mask3),-16)
ilogx   = ishft(iand(istatus,mask4),-17)
ilogy   = ishft(iand(istatus,mask5),-18)

write (iw,"(A,A,A)") 'ax.set(ylabel=r"',ylabel,'")'

istatus = iw + ishft(idx,8) + ishft(ilabel,16) + ishft(ilogx,17) + ishft(ilogy,18)

end subroutine


subroutine pyplot_add_function(istatus,istyle,label,n,xs,ys)
implicit double precision (a-h,o-z)

integer                  :: istyle, idx
character(len=*)         :: label
double precision         :: xs(n),ys(n)
character(len=4)         :: xname,yname
character(:),allocatable :: style

integer               :: istatus
integer, parameter    :: mask1 = Z'000000FF'
integer, parameter    :: mask2 = Z'0000FF00'
integer, parameter    :: mask3 = Z'00010000'
integer, parameter    :: mask4 = Z'00020000'
integer, parameter    :: mask5 = Z'00040000'


iw      = iand(istatus,mask1)
idx     = ishft(iand(istatus,mask2),-8)
ilabel  = ishft(iand(istatus,mask3),-16)
ilogx   = ishft(iand(istatus,mask4),-17)
ilogy   = ishft(iand(istatus,mask5),-18)


idx     = idx+1

write (xname,"(A,I2.2)") "xs",idx
write (yname,"(A,I2.2)") "ys",idx


write(iw,"(A,A,I5,A)") xname," = np.zeros(",n,")"
write(iw,"(A,A,I5,A)") yname," = np.zeros(",n,")"

do i=1,n
write(iw,"(A,A,I5,A,E24.16)") xname,"[",i-1,"] = ",xs(i)
write(iw,"(A,A,I5,A,E24.16)") yname,"[",i-1,"] = ",ys(i)
end do


if (istyle .eq. 1) then
allocate(character(2) :: style)
style='b-'
elseif (istyle .eq. 2) then
allocate(character(2) :: style)
style = "b:"
elseif (istyle .eq. 3) then
allocate(character(3) :: style)
style = "b--"
elseif (istyle .eq. 4) then
allocate(character(3) :: style)
style = "b-."
elseif (istyle .eq. 5) then
allocate(character(2) :: style)
style='r-'
elseif (istyle .eq. 6) then
allocate(character(2) :: style)
style = "r:"
elseif (istyle .eq. 7) then
allocate(character(3) :: style)
style = "r--"
elseif (istyle .eq. 8) then
allocate(character(3) :: style)
style = "r-."
elseif (istyle .eq. 9) then
allocate(character(2) :: style)
style='k-'
elseif (istyle .eq. 10) then
allocate(character(2) :: style)
style = "k:"
elseif (istyle .eq. 11) then
allocate(character(3) :: style)
style = "k--"
elseif (istyle .eq. 12) then
allocate(character(3) :: style)
style = "k-."
endif


if (len(label) .gt. 0) then
write(iw,"(A,A,A,A,A,A,A,A,A,A)") "ax.plot(",xname,",",yname,",'",&
  style,"'",',label = r"',label,'",linewidth=2)'
ilabel = 1
else
write(iw,"(A,A,A,A,A,A,A)") "ax.plot(linewidth=.5,",xname,",",yname,",'",style,&
  ",linewidth=2')"
endif


istatus = iw + ishft(idx,8) + ishft(ilabel,16) + ishft(ilogx,17) + ishft(ilogy,18)


end subroutine


subroutine pyplot_end(istatus,out_name)
implicit double precision (a-h,o-z)

character(len=*)    :: out_name

!
!  Input parameters:
!


integer               :: istatus
integer, parameter    :: mask1 = Z'000000FF'
integer, parameter    :: mask2 = Z'0000FF00'
integer, parameter    :: mask3 = Z'00010000'
integer, parameter    :: mask4 = Z'00020000'
integer, parameter    :: mask5 = Z'00040000'


iw      = iand(istatus,mask1)
idx     = ishft(iand(istatus,mask2),-8)+1
ilabel  = ishft(iand(istatus,mask3),-16)
ilogx   = ishft(iand(istatus,mask4),-17)
ilogy   = ishft(iand(istatus,mask5),-18)


if (ilabel .eq. 1) then
write(iw,"(A)")     "legend=ax.legend(loc='upper left', fontsize='small')"
endif

write(iw,"(A)") 'ax.grid()'
write(iw,"(A,A,A)") 'fig.savefig("',out_name,'")'
write(iw,"(A)") 'plt.show()'

end subroutine



subroutine pyplot_function(script_name,out_name,xlabel,ylabel,n,xs,ys)
implicit double precision (a-h,o-z)

character(len=*)             :: script_name,out_name,xlabel,ylabel
double precision             :: xs(n),ys(n)

!
!  Output a python script which plots a function give a collection
!  of points in its graph.  
!
!  Input parameters:
!    script_name - the name of the python script to generate
!    out_name - the name of the .png image file generated by the script
!    n - the number of point in the graph of the function to be specified
!    xs - a vector of length n speciying the x coordinate of each point
!    ys - a vector of length n speciying th  y coordinate of each point    
!
!  Output parameters:
!    N/A
!
!

iw = 20
open(iw,FILE=script_name)
write(iw,"(A)") "import numpy as np"
write(iw,"(A)") "import matplotlib.pyplot as plt"


write(iw,"(A,I5,A)") "xs = np.zeros(",n,")"
write(iw,"(A,I5,A)") "ys = np.zeros(",n,")"

do i=1,n
write(iw,"(A,I5,A,E24.16)") "xs[",i-1,"] = ",xs(i)
write(iw,"(A,I5,A,E24.16)") "ys[",i-1,"] = ",ys(i)
end do

write(iw,"(A)") "fig, ax = plt.subplots()"

if (len(xlabel) .gt. 0) then
write (iw,"(A,A,A)") 'ax.set(xlabel="',xlabel,'")'
endif

if (len(ylabel) .gt. 0) then
write (iw,"(A,A,A)") 'ax.set(ylabel="',ylabel,'")'
endif

write(iw,"(A)") "ax.plot(xs,ys)"
write(iw,"(A)") 'ax.grid()'
write(iw,"(A,A,A)") 'fig.savefig("',out_name,'")'

!write(iw,"(A)") 'plt.show()'

close(iw)
end subroutine



subroutine write_table_double(iw,xx)
implicit double precision (a-h,o-z)

if (xx .eq. 0) then
i1 = 0
i2 = 0
i3 = 0
nexp = 0

write (iw,'(I1,".",I1,I1,"\e{+",I2.2,"}")',advance="no") i1,i2,i3,nexp

return
endif

nexp = 0
x    = xx

if (x .gt. 1) then

do while (x .ge. 10.0d0) 
x    = x / 10.0d0
nexp = nexp + 1
end do


i1 = floor(x)
i2 = floor(x*10-10*i1)
i3 = floor(x*100-100*i1-10*i2)

write (iw,'(I1,".",I1,I1,"\e{+",I2.2,"} ")',advance="no") i1,i2,i3,nexp

else

do while (x .le. 1.0d0) 
x    = x * 10.0d0
nexp = nexp + 1
end do


i1 = floor(x)
i2 = floor(x*10-10*i1)
i3 = floor(x*100-100*i1-10*i2)

write (iw,'(I1,".",I1,I1,"\e{-",I2.2,"}")',advance="no") i1,i2,i3,nexp

endif

end subroutine





subroutine write_table_integer_range(iw,nu1,nu2)
implicit double precision (a-h,o-z)

call write_table_integer_sep(iw,nu1)
write(iw,"(A)",advance="no") " - "
call write_table_integer_sep(iw,nu2)

end subroutine



subroutine write_table_integer_sep(iw,n)
implicit double precision (a-h,o-z)

integer :: idigits(1000)

m      = n
ncount = 0
isep   = 0

if (n .eq. 0) then
write(iw,"(A)",advance="no") "0"
return
endif

do while (m .ge. 1)

if (isep .eq. 3) then
ncount          = ncount+1
idigits(ncount) = -1
isep            = 0
endif

ncount          = ncount+1
idigits(ncount) = mod(m,10)
m               = m /10
isep            = isep+1

end do

do i=1,ncount
id = idigits(ncount-i+1)
if (id .ge. 0) then
write(iw,"(I1)",advance="no") id
else
write(iw,"(A)",advance="no") "\sep,"
endif

end do

end subroutine


subroutine write_table_integer_power2(iw,n)
implicit double precision (a-h,o-z)


dd = log(n+0.0d0)/log(2.0d0)
if(dd .ge. 10) then
write(iw,"('$2^{',I2,'}$')",advance="no") int(dd)
else
write(iw,"('$2^{',I1,'}$')",advance="no") int(dd)
endif

end subroutine


subroutine write_table_nextline(iw)
implicit double precision (a-h,o-z)
write (iw,*) " \\"
end subroutine


subroutine write_table_next(iw)
implicit double precision (a-h,o-z)
write (iw,"(' & ')",advance="no") 
end subroutine



end module
