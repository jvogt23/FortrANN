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
        real(kind=8), dimension(:,:), allocatable :: backPassGradients
    contains
        procedure :: relu_forward
        procedure :: relu_backward
    end type ReLU

    type :: SoftMax
        real(kind=8), dimension(:,:), allocatable :: outputs
        real(kind=8), dimension(:,:), allocatable :: dInputs
    contains
        procedure :: sm_forward
    end type SoftMax

    type :: LossCal
        real(kind=8) :: loss
        real(kind=8), dimension(:,:), allocatable :: dInputs
    contains
        procedure :: calcCxeLoss
        procedure :: calcDLoss
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

    ! Performs a backward pass of the ReLU activation function.
    ! Requires the layer this ReLU function is attached to and the gradients
    ! produced by the backward pass of the layer in front.
    subroutine relu_backward(self, layer, gradients)
        class(ReLU), intent(inout) :: self
        class(DenseLayer), intent(in) :: layer
        real(kind=8), dimension(:,:), allocatable, intent(in) :: gradients

        if (allocated(self%backPassGradients)) then
            deallocate(self%backPassGradients)
        end if

        allocate(self%backPassGradients(size(gradients, 1), size(gradients, 2)))
        self%backPassGradients = gradients
        where (layer%outputs(:,:).lt.0.0)
            self%backPassGradients(:,:) = 0.0
        end where
    end subroutine relu_backward

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

    ! Calculates dloss wrt to the softmax function's inputs
    ! The reason for doing this is because doing dloss and dSoftmax
    ! separately is more computationally expensive
    ! and more difficult to do.
    ! Doing both of these gradients in one simplifies down to
    ! y_hat - y, which is much easier to do.
    ! Refer to Page 234 of the textbook I am using.
    subroutine calcDLoss(self, smActivator, y_actuals)
        class(LossCal), intent(inout) :: self
        class(SoftMax) intent(in) :: smActivator
        integer, dimension(:), allocatable, intent(in) :: y_actuals
        integer :: i

        if (allocated(self%dInputs)) then
            deallocate(self%dInputs)
        end if
        self%dInputs = smActivator%outputs

        do i = 1, size(smActivator%outputs, 1)
            self%dInputs(i,y_actuals(i)) = self%dInputs(i, y_actuals(i)) - 1
        end do

        self%dInputs = self%dInputs / size(smActivator%outputs, 1)
    end subroutine calcDLoss

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

        self%accuracy = 0

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
        predEqActual = 0
        do i = 1, size(predictions)
            if (predictions(i).eq.actual(i)) then
                predEqActual = predEqActual + 1.0
            end if
        end do
        self%accuracy = (predEqActual / size(predictions))
    end subroutine calcAccuracy
end module activation_fns
