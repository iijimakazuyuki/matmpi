module mat_mess_mod
	implicit none
	type :: mat_mess
		!n 通信相手の数
		!neib 通信相手のプロセッサ番号
		!neib_idx i番目の通信相手に対して通信する節点は neib_idx(i-1)+1, ..., neib_idx(i) の idx
		!idx 通信する節点番号
		!buf, req, stat 通信の際に必要になる諸々の配列
		integer :: n
		integer, allocatable :: neib(:), neib_idx(:), idx(:), req(:), stat(:,:)
		double precision, allocatable :: buf(:)
	end type
contains
	subroutine init_neib_mat_mess(m)
		type(mat_mess), intent(inout) :: m
		allocate(m%neib(m%n), m%neib_idx(0:m%n))
		m%neib_idx = 0
	end subroutine
	
	subroutine init_idx_mat_mess(m)
		type(mat_mess), intent(inout) :: m
		allocate(m%idx(m%neib_idx(m%n)))
	end subroutine
	
	subroutine print_mat_mess(m)
		type(mat_mess), intent(in) :: m
		print *, m%n
		print *, m%neib
		print *, m%neib_idx
		print *, m%idx
	end subroutine
	
	subroutine write_mat_mess(n,m)
		type(mat_mess), intent(in) :: m
		integer :: n
		write(n,*), m%n
		write(n,*), m%neib
		write(n,*), m%neib_idx
		write(n,*), m%idx
	end subroutine
	
	subroutine write_unformatted_mat_mess(n,m)
		type(mat_mess), intent(in) :: m
		integer :: n
		write(n), m%n
		write(n), m%neib
		write(n), m%neib_idx
		write(n), m%idx
	end subroutine
end module
