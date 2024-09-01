! --- Activation Functions --- !
! @author James Vogt
! @date 8/26/2024
! A module that will contain the various activation functions that
! this project will use.
! Currently, the activation functions I know I will use include
! ReLU and Softmax, and I will also house my loss calculations here.
module activation_fns
    use mod_DenseLayer
    use printutils
    use mathy_bois
    implicit none

    type :: ReLU
        real(kind=8), dimension(:,:), allocatable :: outputs
    contains
        procedure :: relu_forward
        !procedure :: relu_backward
    end type ReLU

    type :: SoftMax
        real(kind=8), dimension(:,:), allocatable :: outputs
    contains
        procedure :: sm_forward
        !procedure :: sm_backward
    end type SoftMax

    type :: LossCal
        real(kind=8) :: loss
    contains
        procedure :: calcCxeLoss
    end type LossCal

    type :: AccuracyCal
        real(kind=8) :: accuracy
    contains
        procedure :: calcAccuracy
    end type AccuracyCal


contains
    ! TODO: All of this
    ! ReLU activation function - returns a set of outputs
    ! rectified such that any output value in self%outputs
    ! which is <= 0 is set to 0.
    ! @return mxn matrix of same size as outputs
    subroutine relu_forward(self, layer)
        class(ReLU), intent(inout) :: self
        class(DenseLayer), intent(in) :: layer
        integer :: i, j

        if (allocated(self%outputs)) then
            deallocate(self%outputs)
        end if

        allocate(self%outputs(size(layer%outputs, 1), size(layer%outputs, 2)))

        do i = 1, size(layer%outputs, 1)
            do j = 1, size(layer%outputs, 2)
                self%outputs(i, j) = max(layer%outputs(i, j), 0.0)
            end do
        end do
    end subroutine relu_forward


    ! Performs a forward pass of the softmax function.
    ! Softmax turns a given matrix into a probability matrix
    subroutine sm_forward(self, layer)
        class(SoftMax), intent(inout) :: self
        class(DenseLayer), intent(in) :: layer
        real(kind=8), dimension(:,:), allocatable :: maxes, ret

        if(allocated(self%outputs)) then
            deallocate(self%outputs)
        end if

        allocate(self%outputs(size(layer%outputs, 1), size(layer%outputs, 2)))
        allocate(maxes(size(layer%outputs, 1), size(layer%outputs, 2)))
        allocate(ret(size(layer%outputs, 1), size(layer%outputs, 2)))
        maxes = spread(maxval(layer%outputs, 2), 2, size(maxes, 2))
        ret = e_raise(layer%outputs - maxes)
        ret = ret / (spread(sum(ret, 2), 2, size(maxes, 2)))
        self%outputs = ret
        !call printMatrixNonAlloc(ret)
    end subroutine sm_forward

    ! Performs a forward pass of calculating Categorical Cross-Entropy Loss
    ! Assumes the 'actual' argument is a single dimension array containing
    ! the index for each row of 'outputs' at which the correct answer is stored.
    subroutine calcCxeLoss(self, outputs, actual)
        class(LossCal), intent(inout) :: self
        real(kind=8), dimension(:,:), allocatable, intent(in) :: outputs
        integer, dimension(:), allocatable, intent(in) :: actual
        real(kind=8), dimension(:), allocatable :: sample_losses
        real(kind=8), dimension(:,:), allocatable :: pred_clipped
        integer :: i, j

        if (.not.allocated(outputs).or..not.allocated(actual)) then
            print *, "Passed in arguments are unallocated or point to null"
            stop
        end if

        if (size(outputs, 1) /= size(actual)) then
            print *, "Argument outputs must have equal rows to size of actual"
            stop
        end if

        allocate(sample_losses(size(actual)))
        allocate(pred_clipped(size(outputs, 1), size(outputs, 2)))
        ! start by clipping the predictions to avoid 0 and 1 being used
        ! in calculations
        do i = 1, size(outputs, 1)
            do j = 1, size(outputs, 2)
                if (outputs(i, j) > (1.0 - 1.0E-7)) then
                    pred_clipped(i, j) = (1.0 - 1.0E-7)
                else if (outputs(i, j) < 1.0E-7) then
                    pred_clipped(i, j) = 1.0E-7
                else
                    pred_clipped(i, j) = outputs(i, j)
                end if
            end do
        end do

        ! Now get the sample losses by grabbing only the correct
        ! confidence value from each row of the outputs
        ! and doing a negative log function on all values
        do i = 1, size(actual)
            sample_losses(i) = -1.0 * dlog(pred_clipped(i, actual(i)))
        end do

        ! Finally, find the mean of the values in sample_losses
        self%loss = sum(sample_losses) / size(sample_losses)

    end subroutine calcCxeLoss

    ! calculates the accuracy of a probability matrix against an array
    ! where each index contains the column where the highest probability
    ! should be stored in the matrix for the given row. Assumes actual is
    ! a single-dimension array.
    subroutine calcAccuracy(self, outputs, actual)
        class(AccuracyCal), intent(inout) :: self
        real(kind=8), dimension(:,:), allocatable, intent(in) :: outputs
        integer, dimension(:), allocatable, intent(in) :: actual
        integer, dimension(:), allocatable :: predictions
        integer :: i
        real(kind=8) :: predEqActual = 0.0

        if (.not.allocated(outputs).or..not.allocated(actual)) then
            print *, "Passed in arguments are unallocated or point to null"
            stop
        end if

        if (size(outputs, 1) /= size(actual)) then
            print *, "Argument outputs must have equal rows to size of actual"
            stop
        end if

        allocate(predictions(size(outputs, 1)))
        predictions = maxloc(outputs, 2)

        do i = 1, size(predictions)
            if (predictions(i).eq.actual(i)) then
                predEqActual = predEqActual + 1
            end if
        end do
        self%accuracy = predEqActual / size(predictions)
    end subroutine calcAccuracy
end module activation_fns
