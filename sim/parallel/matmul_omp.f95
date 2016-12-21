program par_matmul

use omp_lib
implicit none
integer, parameter :: n = 1000
!$ integer :: num_threads = 4

integer :: i, j, k
integer, dimension(n,n) :: a, b, c
real :: t1, t2, ep

!$ print *, "Number of threads used : ",num_threads

!$ call omp_set_num_threads(num_threads)

a = 1
b = 0
do i = 1,n
	b(i,i) = 1
end do
c = 0

call cpu_time(t1)

!$omp parallel do private(j,k)
do i = 1,n
	do j = 1,n
		do k = 1,n
			c(i,j) = c(i,j) + a(i,k)*b(k,j)
		end do
	end do
end do

!$omp end parallel do

call cpu_time(t2)

ep = t2 - t1

!$ ep = ep/num_threads

print *, "Time", ep

end program par_matmul
