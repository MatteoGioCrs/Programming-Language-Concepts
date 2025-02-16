! Project 01
! CSE4250 Programming Language Concepts
! Matteo Caruso, Emma Bahr, Joshua Cajuste

program solve_problem
    implicit none
    integer :: N, t, i
    integer, allocatable :: A(:)

    read(*,*) N, t
    allocate(A(N))
    read(*,*) A

    select case (t)
        case (1)
            call task1(N, A)
        case (2)
            call task2(N, A)
        case (3)
            call task3(N, A)
        case (4)
            call task4(N, A)
        case (5)
            call task5(N, A)
    end select

    deallocate(A)
end program solve_problem

subroutine task1(N, A)
    implicit none
    integer, intent(in) :: N
    integer, intent(in) :: A(N)
    integer :: i, j
    logical :: found

    found = .false.
    do i = 1, N
        do j = i + 1, N
            if (A(i) + A(j) == 7777) then
                print *, 'Yes'
                return
            end if
        end do
    end do
    print *, 'No'
end subroutine task1

subroutine task2(N, A)
    implicit none
    integer, intent(in) :: N
    integer, intent(in) :: A(N)
    integer :: i, j

    do i = 1, N
        do j = i + 1, N
            if (A(i) == A(j)) then
                print *, 'Contains duplicate'
                return
            end if
        end do
    end do
    print *, 'Unique'
end subroutine task2

subroutine task3(N, A)
    implicit none
    integer, intent(in) :: N
    integer, intent(in) :: A(N)
    integer :: i, j, count

    do i = 1, N
        count = 0
        do j = 1, N
            if (A(i) == A(j)) count = count + 1
        end do
        if (count > N / 2) then
            print *, A(i)
            return
        end if
    end do
    print *, -1
end subroutine task3

subroutine task4(N, A)
    implicit none
    integer, intent(inout) :: A(N)
    integer, intent(in) :: N
    integer :: mid

    call quicksort(A, 1, N)
    if (mod(N, 2) == 1) then
        print *, A((N+1)/2)
    else
        print *, A(N/2), A(N/2 + 1)
    end if
end subroutine task4

subroutine task5(N, A)
    implicit none
    integer, intent(in) :: N
    integer, intent(inout) :: A(N)
    integer :: i

    call quicksort(A, 1, N)
    do i = 1, N
        if (A(i) >= 100 .and. A(i) <= 999) then
            print *, A(i), ' ',
        end if
    end do
    print *
end subroutine task5

subroutine quicksort(A, low, high)
    implicit none
    integer, intent(inout) :: A(:)
    integer, intent(in) :: low, high
    integer :: pivot, i, j, temp

    if (low < high) then
        pivot = A(high)
        i = low - 1
        do j = low, high - 1
            if (A(j) <= pivot) then
                i = i + 1
                temp = A(i)
                A(i) = A(j)
                A(j) = temp
            end if
        end do
        temp = A(i + 1)
        A(i + 1) = A(high)
        A(high) = temp
        call quicksort(A, low, i)
        call quicksort(A, i + 2, high)
    end if
end subroutine quicksort
