module mat_mpi
	use mat_mess_mod
	use mpi
	implicit none
contains
	function read_file_mat_mess(in) result(m)
		type(mat_mess) :: m
		integer, intent(in) :: in
		read(in,*) m%n
		allocate(m%neib(m%n), m%neib_idx(0:m%n))
		read(in,*) m%neib
		read(in,*) m%neib_idx
		allocate(m%idx(m%neib_idx(m%n)), m%buf(m%neib_idx(m%n)))
		read(in,*) m%idx
		
		allocate(m%req(m%n), m%stat(MPI_STATUS_SIZE, m%n))
	end function
end module
