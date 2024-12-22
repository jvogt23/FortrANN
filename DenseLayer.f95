module mod_DenseLayer
    use mathy_bois
    implicit none

    type :: DenseLayer
        integer :: nNeurons, nInputs
        real(kind=8), dimension(:,:), allocatable :: weights
        real(kind=8), dimension(:), allocatable :: biases
        real(kind=8), dimension(:,:), allocatable :: inputs
        real(kind=8), dimension(:,:), allocatable :: outputs
        real(kind=8), dimension(:,:), allocatable :: dweights
        real(kind=8), dimension(:,:), allocatable :: dinputs
        real(kind=8), dimension(:), allocatable :: dbiases
    contains
        procedure :: init
        procedure :: forward
        procedure :: backward
    end type DenseLayer

contains
    ! Initializes a new DenseLayer.
    ! @param nInputs - The number of inputs expected of each neuron
    ! @param nNeurons - The number of neurons in the layer
    subroutine init(self, nInputs, nNeurons)
        class(DenseLayer) :: self
        integer, intent(in) :: nInputs, nNeurons
        integer :: i, j
        real(kind=8), dimension(nInputs, nNeurons) :: tempWeights
        real(kind=8), dimension(nNeurons) :: tempBiases
        self%weights = tempWeights
        self%biases = tempBiases
        self%biases = 0.0
        do i = 1, nInputs
            do j = 1, nNeurons
                self%weights(i, j) = 0.01 * rand_normal()
            end do
        end do
        self%nNeurons = nNeurons
        self%nInputs = nInputs
    end subroutine init

    ! Performs a forward pass through the layer.
    ! @param inputs - A matrix of input values. Must have
    !                 The same number of columns as the rows in weights.
    subroutine forward(self, input)
        class(DenseLayer) :: self
        real(kind=8), dimension(:,:), intent(in) :: input

        if (allocated(self%inputs)) then
            deallocate(self%inputs)
        end if
        allocate(self%inputs(size(input, 1), size(input, 2)))
        self%inputs = input
        self%outputs = matmul(input, self%weights)
    end subroutine forward

    ! Performs a backward pass through the layer.
    ! Produces values for dweights, dbiases, and dinputs.
    ! Requires the results for a backward pass through the layer's activator
    subroutine backward(self, dAct)
        class(DenseLayer), intent(inout) :: self
        real(kind=8), dimension(:,:), allocatable, intent(in) :: dAct

        if (.not.allocated(dAct)) then
            print *, "Parameter dAct must be allocated."
            stop
        end if

        if (allocated(self%dweights)) then
            deallocate(self%dweights)
        end if

        if (allocated(self%dinputs)) then
            deallocate(self%dinputs)
        end if

        if (allocated(self%dbiases)) then
            deallocate(self%dbiases)
        end if

        allocate(self%dbiases(size(self%biases)))
        allocate(self%dweights(size(transpose(self%inputs), 1), size(dAct, 2)))
        allocate(self%dinputs(size(dAct, 1), size(transpose(self%weights), 2)))

        self%dbiases = sum(dAct, 1)
        self%dweights = matmul(transpose(self%inputs), dAct)
        self%dinputs = matmul(dAct, transpose(self%weights))
    end subroutine backward

end module mod_DenseLayer
