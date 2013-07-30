module spmv_mpi
	use mpi
	use omp_lib
	use matcrs_mod
	use mat_mpi
	implicit none
contains
	subroutine spmv(Ap,x,send,recv)
		type(matcrs), intent(in) :: Ap
		double precision, intent(inout) :: x(:)
		type(mat_mess), intent(inout) :: send, recv
		
		integer :: i, j, k, si, ei, err
		
		x = Ap*x
		
		do i=1, send%n
			si = send%neib_idx(i-1)+1
			ei = send%neib_idx(i)
			do j=si, ei
				send%buf(j) = x(send%idx(j))
			end do
			call mpi_isend(send%buf(si), ei-si+1, MPI_DOUBLE_PRECISION,&
				send%neib(i)-1, 0, MPI_COMM_WORLD, send%req(i), err)
		end do
		
		do i=1, recv%n
			si = recv%neib_idx(i-1)+1
			ei = recv%neib_idx(i)
			call mpi_irecv(recv%buf(si), ei-si+1, MPI_DOUBLE_PRECISION,&
				recv%neib(i)-1, 0, MPI_COMM_WORLD, recv%req(i), err)
		end do
		
		call mpi_waitall(recv%n, recv%req, recv%stat, err)
		
		do i=1, recv%n
			do j=recv%neib_idx(i-1)+1, recv%neib_idx(i)
				x(recv%idx(j)) = recv%buf(j)
			end do
		end do
		
		call mpi_waitall(send%n, send%req, send%stat, err)
		
	end subroutine
end module
