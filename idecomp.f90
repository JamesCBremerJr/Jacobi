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

module idecomp

use utils

contains

subroutine idecomp_construct(eps,a,krank,ipivs,r)
implicit double precision (a-h,o-z)

double precision               :: eps,a(:,:)
integer, allocatable           :: ipivs(:)
double precision, allocatable  :: r(:,:)

!
!
!

! double precision, allocatable :: c(:,:),q(:,:),rnorms(:),work(:),rinv(:,:),r0(:,:)
! integer, allocatable          :: idxs(:)

double precision, allocatable :: q(:,:),rnorms(:),c(:,:),rinv(:,:),work(:),r2(:,:),r0(:,:)

n   = size(a,1)
m   = size(a,2)

allocate(rnorms(n+m+1000),q(n,m),ipivs(m))

q = a
call gspiv(q,n,m,eps,rnorms,ipivs,krank)
! call prini("krank = ",krank)
! call prin2("rnorms = ",rnorms(1:krank))


call insorti(krank,ipivs(1:krank))
 ! call prini("ipivs = ",ipivs(1:krank))

allocate(r2(krank,m),c(n,m),rinv(krank,krank),work(krank*krank*8 + 1000))

c = a(:,ipivs)

r2 = matmul(transpose(q(:,1:krank)),c)
rinv = r2(1:krank,1:krank)
call orthom(rinv,krank,work,cond)



allocate(r(krank,m),r0(krank,m))
r0 = matmul(rinv,r2)

do j=1,m
r(:,ipivs(j)) = r0(:,j)
end do


end subroutine


end module
