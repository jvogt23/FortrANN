! File IO
! Handles input/output for files within this project.

module CsvFileIO
    use printutils
    implicit none
    ! NOTE: Since Fortran 95 has really weird rules for generics and stuff
    ! (As far as I know, it may just not have that stuff)
    ! I can't accept different types of data here, so assume that CSVs use
    ! real numbers for the purposes of this program
    ! (Trust me, I would make this generic if I knew how. Some day!)
    type :: CsvFile
        integer :: iounit, numCol, numRow, ioS
        real(kind=8), dimension(:,:), allocatable :: dataSet
        character(:), allocatable :: rawData
    contains
        procedure :: openCsvFile
        procedure :: findRows
        procedure :: findCols
    end type CsvFile
contains
    ! Goes through entire process of opening a CSV file containing floating point
    ! numbers. Assumes the data in the CSV consists of floating point numbers only
    ! and translates the data to a 2D array. The 2D array is what the user should
    ! read from and manipulate.
    !@param fileName - The name of the file to open for this CSV
    subroutine openCsvFile(self, fileName)
        ! data
        class(CsvFile), intent(inout) :: self
        character(*), intent(in) :: fileName
        integer :: ioS, fileSize, i, j, k
        character(256), dimension(:), allocatable :: splitRawData ! sorry about the assumed length; I only need this to work once
        logical :: OK
        real(kind=8) :: dummy
        ! procedure
        inquire(file=fileName, exist=OK, size=fileSize)
        if (OK) then
            open(newunit=self%iounit, file=fileName, status='OLD', form='UNFORMATTED', access='STREAM', iostat=ioS)
            if (ioS /= 0) then
                print *, "IO Error occurred when opening CSV file."
                self%ioS = 1
                return
            end if
        else
            print *, "No file exists at specified path."
            self%ioS = 2
            return
        end if
        allocate(character(len=fileSize) :: self%rawData)
        read(self%iounit, pos=1, iostat=ioS) self%rawData
        if (ioS /= 0) then
            print *, "IO Error occurred when reading CSV file."
            self%ioS = 1
            return
        end if
        !print *, fileSize
        !print *, self%rawData
        call self%findCols()
        call self%findRows()
        allocate(splitRawData(self%numRow * self%numCol))
        read(self%rawData, *) splitRawData
        allocate(self%dataSet(self%numRow, self%numCol))
        k = 1
        do i = 1, self%numRow
            do j = 1, self%numCol
                read(splitRawData(k), *) dummy
                self%dataSet(i, j) = dummy
                k = k + 1
            end do
        end do
        !call printMatrixNonAlloc(self%dataSet)
        self%ioS = 0
    end subroutine openCsvFile

    ! Finds the rows in the CSV and outputs to self%numRow
    ! This subroutine only really needs to be run when opening a CSV, but I
    ! extracted to a subroutine to make the openCsvFile subroutine more brief.
    subroutine findRows(self)
        ! data
        class(CsvFile), intent(inout) :: self
        integer :: numRow = 0
        ! procedure
        if (.not.allocated(self%rawData)) then
            print *, "CSV File not open/allocated."
            self%ioS = 1
            return
        end if
        call countSubstrings(self%rawData, achar(10), numRow)
        if (numRow.eq.0) call countSubstrings(self%rawData, achar(13)//achar(10), numRow)
        self%numRow = numRow
        if (self%rawData(len(self%rawData):len(self%rawData)) == achar(10)) self%numRow = self%numRow + 1
    end subroutine findRows

    ! Finds the columns in a CSV and outputs to self%numCol
    ! Only really needs to be run when opening the CSV, but
    ! is extracted to make openCsvFile more brief.
    subroutine findCols(self)
        !data
        class(CsvFile), intent(inout) :: self
        integer :: rowEndIndex = 0
        character :: newLine = achar(10)
        character :: carriageReturn = achar(13) ! Normally won't need this
        ! procedure
        if (.not.allocated(self%rawData)) then
            print *, "CSV File not open/allocated."
            self%ioS = 1
            return
        end if
        rowEndIndex = index(self%rawData, newLine)
        if (rowEndIndex.eq.0) then
            rowEndIndex = index(self%rawData, carriageReturn//newLine)
            if (rowEndIndex.eq.0) then
                print *, "No newlines detected, may be invalid format or not a CSV."
                self%ioS = 1
                return
            end if
        end if

        call countSubstrings(self%rawData(1:rowEndIndex), ',', self%numCol)
        if(len(self%rawData) > 0) self%numCol = self%numCol + 1
    end subroutine findCols

end module CsvFileIO
