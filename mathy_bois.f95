! --- Mathy Bois --- !
! Written by: James Vogt
! Date: 07-28-2024
! This module contains any functions I needed but couldn't find anywhere
! for mathematical things.
! Given that this is Fortran, it will probably be pretty short.
! (Watch as I eat my words)

module mathy_bois
    implicit none
    ! Constants
    real(kind=8) :: pi = 3.141592653589793
    real(kind=8) :: e  = 2.718281828459045

contains

    ! uses a Box-Muller transform to change a uniform
    ! distribution random number into a normal Gaussian distribution
    ! random number.
    ! @return a real random number within the normal distribution
    function rand_normal()
        real(kind=8) :: U_1, U_2
        real(kind=8) :: rand_normal
        call random_number(U_1)
        call random_number(U_2)
        U_1 = abs(U_1)
        U_2 = abs(U_2)

        rand_normal = sqrt(-2 * log(U_1)) * cos(2 * pi * U_2)
    end function rand_normal

    function rand_normal_mat(dim1, dim2)
        integer, intent(in) :: dim1
        integer, intent(in) :: dim2
        real(kind=8), dimension(:,:), allocatable :: rand_normal_mat
        integer :: i, j

        allocate(rand_normal_mat(dim1, dim2))
        do i = 1, dim1
            do j = 1, dim2
                rand_normal_mat(i, j) = rand_normal()
            end do
        end do

    end function rand_normal_mat

    function rand_normal_arr(arrsize)
        integer, intent(in) :: arrsize
        real(kind=8), dimension(:), allocatable :: rand_normal_arr
        integer :: i

        allocate(rand_normal_arr(arrsize))
        do i = 1, arrsize
            rand_normal_arr(i) = rand_normal()
        end do
    end function rand_normal_arr

    function e_raise(mat)
        real(kind=8), dimension(:,:) :: mat
        real(kind=8) :: e_raise(size(mat, 1), size(mat, 2))
        e_raise = e ** mat
    end function e_raise

    function diagflat(mat)
        real(kind=8), dimension(:), intent(in) :: mat
        real(kind=8), dimension(:,:), allocatable :: diagflat
        integer :: i

        allocate(diagflat(size(mat), size(mat)))
        do i = 1, size(mat)
            diagflat(i, i) = mat(i)
        end do
    end function diagflat

end module mathy_bois
