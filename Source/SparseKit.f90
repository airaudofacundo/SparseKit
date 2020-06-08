  !*************************************************************
  !           Instituto Universitario Aeronautico
  !                Dpto. Mecanica Aeronautica
  !*************************************************************
  ! Filename      : SparseKit.f90
  ! Version       : 0.9
  ! Date          : 14-08-2019
  ! Programmer(s) : F. Airaudo(fairaudo574@alumnos.iua.edu.ar)
  !                 M. Zuñiga(mzuniga433@alumnos.iua.edu.ar)
  !*************************************************************
  ! Description (brief):
  !                     Module for all algebraic and functional
  !                     Operations with Sparse Matrices. That is,
  !                     For matrices where the number of nonzeros
  !                     highly exceeds the number of zeros, this
  !                     tools allow you to declare a derived
  !                     data type to work with them. 
  !************************************************************* 
  ! Dependecies:
  !             Use tools        - Filename: Utilities.f90
  !             Use QuickSortMod - Filename: Quicksort.f90
  !*************************************************************
  ! Public procedures:
  !              Type(Sparse)
  !              Derived type Procedures 
  !                   Subroutine init
  !                   Subroutine update
  !                   Subroutine append
  !                   Subroutine makeCRS
  !                   Subroutine appendPostCRS
  !                   Subroutine change
  !                   Subroutine setDirichlet
  !                   Function get
  !                   Function getn
  !                   Function getNonZeros
  !                   Subroutine printValue
  !                   Subroutine printNonZeros
  !                   Subroutine printAll
  !                   Subroutine deleteRowAndCol
  !                   Subroutine free
  !              Module Procedures:
  !                   Function transpose
  !                   Function norm
  !                   Function trace
  !                   Function id
  !                   Subroutine sparse_sparse_prod ->  Operator:
  !                   Subroutine coef_sparse_prod   ->    
  !                   Subroutine sparse_vect_prod   ->     (*)
  !                   Subroutine sparse_sparse_add  ->  Operator:
  !                                                        (+)
  !                   Subroutine sparse_sparse_sub  ->  Operator:
  !                                                        (-)
  !*************************************************************
include 'mkl_pardiso.f90'  
module SparseKit

  !***********************************************
  !*                 EXTERNS                     *
  !***********************************************
  use UtilitiesM
  use quickSortM
  use mkl_pardiso
  
  implicit none
  
  private
  public :: Sparse, operator(*), operator(+), operator(-), transpose&
       , norm, trace, id, inverse, inverseLumped
  
  type Triplet
     real(rkind)   , dimension(:), allocatable :: A
     integer(ikind), dimension(:), allocatable :: row
     integer(ikind), dimension(:), allocatable :: col
  end type Triplet
  
  type Sparse
     private
     real(rkind)   , dimension(:), allocatable :: A
     integer(ikind), dimension(:), allocatable :: AI
     integer(ikind), dimension(:), allocatable :: AJ
     integer(ikind), dimension(:), allocatable :: rowCounter
     integer(ikind)                            :: n
     integer(ikind)                            :: nnz
     integer(ikind)                            :: counter
     type(triplet)                             :: triplet
     logical                                   :: isCRSDone
     
   contains
     
     procedure, public  :: init
     procedure, public  :: update
     procedure, public  :: append
     procedure, public  :: makeCRS

     procedure, public  :: appendPostCRS
     procedure, public  :: change
     procedure, public  :: setDirichlet

     procedure, public  :: get
     procedure, public  :: getnnz
     procedure, public  :: getn
     procedure, public  :: getA
     procedure, public  :: getAI
     procedure, public  :: getAJ

     procedure, public  :: printValue
     procedure, public  :: printNonZeros
     procedure, public  :: printAll

     procedure, public  :: deleteRowAndCol

     procedure, public  :: free

     procedure, private :: handleDuplicates
  end type Sparse

  interface sparse
     procedure constructor
  end interface sparse

  interface operator(*)
     module procedure sparse_sparse_prod
     module procedure sparse_vect_prod
     module procedure coef_sparse_prod
  end interface operator(*)
  
  interface operator(+)
     module procedure sparse_sparse_add
  end interface operator(+)
  
  interface operator(-)
     module procedure sparse_sparse_sub
  end interface operator(-)
  
  interface transpose
     module procedure transpose
  end interface transpose
  
  interface norm
     module procedure norm
  end interface norm
  
  interface trace
     module procedure trace
  end interface trace

  interface id
     module procedure id
  end interface id

  interface inverse
     module procedure inverse
  end interface inverse

  interface inverseLumped
     module procedure inverseLumped
  end interface inverseLumped
  
  !***********************************************
  !*          LOCAL PRIVATE VARIABLES            *
  !***********************************************
  real(rkind), dimension(:), allocatable :: valueVector
  real(rkind), dimension(:), allocatable :: auxA
  integer(ikind), dimension(:), allocatable :: auxAJ
  integer(ikind), dimension(:), allocatable :: rowVector
  integer(ikind) :: repeats, l, i

contains
  
  !***************************************************
  ! Constructor:
  !     Initializes the object with and estimate of
  !     the matrix's non zeros and the number of rows
  !     Won't work if given an inferior nnz
  !  
  ! Parameters:
  !     Input, nnz(int), rows(int)
  !     Output, -
  !***************************************************
  type(Sparse) function constructor(nnz, rows)
    implicit none
    integer(ikind), intent(in) :: nnz
    integer(ikind), intent(in) :: rows
    call constructor%init(nnz, rows)
  end function constructor
  subroutine init(this, nnz, rows)
    implicit none
    class(Sparse), intent(inout) :: this
    integer(ikind), intent(in) :: nnz
    integer(ikind), intent(in) :: rows
    this%nnz = nnz
    this%n = rows
    allocate(this%triplet%A(this%nnz))
    allocate(this%triplet%row(this%nnz))
    allocate(this%triplet%col(this%nnz))
    this%triplet%A = 0
    this%triplet%row = 0
    this%triplet%col = 0
    this%counter = 0
    this%isCRSDone = .false.
  end subroutine init
  
  !***************************************************
  ! update:
  !     updates basic values
  !  
  ! Parameters:
  !     Input, nnz(int), rows(int)
  !     Output, -
  !***************************************************
  subroutine update(this, nnz, rows)
    implicit none
    class(sparse), intent(inout) :: this
    integer(ikind), intent(in) :: nnz
    integer(ikind), intent(in) :: rows
    this%isCRSDone = .false.
    if(allocated(this%triplet%A)) then
       deallocate(this%triplet%A, this%triplet%row, this%triplet%col)
    end if
    if(allocated(this%A)) then
       deallocate(this%A, this%AI, this%AJ)
    end if
    this%nnz = nnz
    this%n = rows
    allocate(this%triplet%A(this%nnz))
    allocate(this%triplet%row(this%nnz))
    allocate(this%triplet%col(this%nnz))
    this%triplet%A = 0
    this%triplet%row = 0
    this%triplet%col = 0
    this%counter = 0
    this%isCRSDone = .false.
  end subroutine update
  
  !***************************************************
  ! append:
  !     takes values one by one and appends it to a
  !     triplet format
  !  
  ! Parameters:
  !     Input, value(realrkind), row(int), col(int)
  !     Output, -
  !***************************************************
  subroutine append(this, val, row, col)
    implicit none
    class(Sparse), intent(inout) :: this
    real(rkind), intent(In) :: val
    integer(ikind), intent(In) :: row, col
    if(this%isCRSDone) call this%appendPostCRS(val, row, col)
    this%counter = this%counter + 1
    this%triplet%A(this%counter) = val
    this%triplet%row(this%counter) = row
    this%triplet%col(this%counter) = col
  end subroutine append
  
  !***************************************************
  ! makeCRS:
  !     Once all values have been appended to the
  !     triplet call this routine to make the CRS
  !  
  ! Parameters:
  !     Input, -
  !     Output, CRS is usable
  !***************************************************
  subroutine makeCRS(this, sortRows)
    implicit none
    class(Sparse), intent(inout) :: this
    logical, intent(in), optional :: sortRows
    integer(ikind) :: i
    this%isCRSDone = .true.
    allocate(this%rowCounter(this%n))
    !This%Counter entries in each row, including duplicates
    this%rowCounter = 0
    do i = 1, this%counter
       this%rowCounter(this%triplet%row(i)) = this%rowCounter(this%triplet%row(i)) + 1
    end do
    !Allocate auxA and auxAJ with nnz
    allocate(auxA(this%counter), auxAJ(this%counter))
    !Order A and AJ
    if(.not.present(sortRows) .or. sortRows) then
       call quicksort(this%triplet%row, this%triplet%col, this%triplet%A, 1, this%counter)
    end if
    !sum up duplicates
    call this%handleDuplicates()
    !Allocate A and AJ with nnz
    if(allocated(this%A)) deallocate(this%A)
    if(allocated(this%AJ)) deallocate(this%AJ)
    allocate(this%A(this%counter), this%AJ(this%counter))
    do i = 1, this%counter
       this%A(i) = auxA(i)
       this%AJ(i) = auxAJ(i)
    end do
    !Counstruct row pointers
    allocate(this%AI(this%n+1))
    this%AI = 1
    do i = 2, this%n+1
       this%AI(i) = this%AI(i-1) + this%rowCounter(i-1)
    end do
    deallocate(this%triplet%A, this%triplet%row, this%triplet%col)
    deallocate(auxA, auxAJ)
    deallocate(this%rowCounter)
    this%nnz = size(this%AJ)
  end subroutine makeCRS
  
  !***************************************************
  ! handleDuplicates:
  !     Sums up every duplicate on the triplet, also
  !     orders values in each row
  !  
  ! Parameters:
  !     Input, -
  !     Output, -
  !***************************************************
  subroutine handleDuplicates(this)
    implicit none
    class(Sparse), intent(inout) :: this
    logical :: mask
    integer(ikind) :: i, j, k
    allocate(rowVector(maxval(this%rowCounter)))
    allocate(valueVector(maxval(this%rowCounter)))
    this%counter = 1
    repeats = 0
    do i = 1, this%n
       rowVector = this%triplet%col(this%counter+repeats:this%counter+repeats+this%rowCounter(i)-1)
       valueVector = this%triplet%A(this%counter+repeats:this%counter+repeats+this%rowCounter(i)-1)
       call quicksort(rowVector, valueVector, 1, this%rowCounter(i))
       j = 0
       do while(j < this%rowCounter(i))
          j = j + 1
          auxA(this%counter) = valueVector(j)
          auxAJ(this%counter) = rowVector(j)
          k = j
          do while(k < this%rowCounter(i))
             k = k + 1
             mask = rowVector(j).eq.rowVector(k)
             if(mask) then
                !add values from k to j
                auxA(this%counter) = auxA(this%counter) + valueVector(k)
                !move k to the back
                call swap(rowVector(k), rowVector(this%rowCounter(i)))
                call swap(valueVector(k), valueVector(this%rowCounter(i)))
                this%rowCounter(i) = this%rowCounter(i) - 1
                repeats = repeats + 1
                k = k - 1
             end if
          end do
          this%counter = this%counter + 1
       end do
       call quicksort(auxAJ, auxA, this%counter-this%rowCounter(i), this%counter-1)
    end do
    this%counter = this%counter - 1
    deallocate(rowVector)
    deallocate(valueVector)
  end subroutine HandleDuplicates

  !***************************************************
  ! appendPostCRS:
  !     Appends a value to the matrix, if that space
  !     is allocated, said value is added to the
  !     existing one
  !  
  ! Parameters:
  !     Input, val(realrkind), row(int), col(int)
  !***************************************************
  subroutine appendPostCRS(this, val, row, col)
    implicit none
    class(Sparse) , intent(inout) :: this
    real(rkind)   , intent(in)    :: val
    integer(ikind), intent(in)    :: row
    integer(ikind), intent(in)    :: col
    integer(ikind) :: index
    logical :: positionExists = .false.
    index = this%AI(row)
    do while(index < this%AI(row+1))
       if(this%AJ(index) == col) then
          positionExists = .true.
          this%A(index) = this%A(index) + val
       end if
       index = index + 1
    end do
    if(.not.positionExists) then
       print'(A)', '** Attempted to append a new value after making the CRS, implementation soon maybe **'
    end if
  end subroutine appendPostCRS

  !***************************************************
  ! change:
  !     Changes a value in the matrix
  !  
  ! Parameters:
  !     Input, val(realrkind), row(int), col(int)
  !***************************************************
  subroutine change(this, val, row, col)
    implicit none
    class(Sparse), intent(inout) :: this
    real(rkind), intent(in) :: val
    integer(ikind), intent(in) :: row
    integer(ikind), intent(in) :: col
    integer(ikind) :: index
    logical :: positionExists = .false.
    index = this%AI(row)
    do while(index < this%AI(row+1))
       if(this%AJ(index) == col) then
          positionExists = .true.
          this%A(index) = val
       end if
       index = index + 1
    end do
    if(.not.positionExists) then
       print'(A)', '** Attempted to change the value in a position not allocated before making the CRS, implementation soon maybe **'
    end if
  end subroutine change

  !***************************************************
  ! setDirichlet:
  !     On a given row, nulls all the values except
  !     the one on the diagonal, which is turned to 1
  !  
  ! Parameters:
  !     Input, row(int)
  !***************************************************
  subroutine setDirichlet(this, row)
    implicit none
    class(Sparse), intent(inout) :: this
    integer(ikind), intent(in) :: row
    integer(ikind) :: index
    index = this%AI(row)
    do while(index < this%AI(row+1))
       this%A(index) = 0.d0
       if(this%AJ(index) == row) then
          this%A(index) = 1.d0
       end if
       index = index + 1
    end do
  end subroutine setDirichlet
  
  !***************************************************
  ! get:
  !     Gives sparse matrix's value from row i
  !     and col j
  !  
  ! Parameters:
  !     Input, i(int), j(int)
  !     Output, get(i,j)(realrkind)
  !***************************************************
  real(rkind) function get(this, i, j)
    implicit none
    class(Sparse), intent(inout) :: this
    integer(ikind), intent(in) :: i
    integer(ikind), intent(in) :: j
    integer(ikind) :: k
    k = this%AI(i)
    do while(k < this%AI(i+1))
       if(this%AJ(k) == j) then
          get = this%A(k)
          return
       end if
       k = k + 1
    end do
    get = 0.0_rkind
  end function get
  
  !***************************************************
  ! getnnz:
  !     given ammount of non zeros
  !  
  ! Parameters:
  !     Input, -
  !     Output, getnnz()(integer(ikind))
  !***************************************************
  integer(ikind) function getnnz(this)
    implicit none
    class(Sparse), intent(inout) :: this
    getnnz = this%nnz
  end function getnnz
  
  !***************************************************
  ! getn:
  !     get order of matrix
  !  
  ! Parameters:
  !     Input, -
  !     Output, getn()(integer(ikind))
  !***************************************************
  integer(ikind) function getn(this)
    implicit none
    class(Sparse), intent(inout) :: this
    getn = this%n
  end function getn

  !***************************************************
  ! getA:
  !     get A vector of matrix
  !  
  ! Parameters:
  !     Input, -
  !     Output, getA(:)(Real(rkind))
  !***************************************************
  function getA(this)
    implicit none
    class(Sparse), intent(inout)    :: this
    real(rkind), dimension(this%nnz) :: getA
       getA = this%A
  end function getA

  !***************************************************
  ! getAI:
  !     get AI vector of matrix
  !  
  ! Parameters:
  !     Input, -
  !     Output, getAI(:)(Integer(ikind))
  !***************************************************
  function getAI(this)
    implicit none
    class(Sparse), intent(inout)       :: this
    integer(ikind), dimension(this%n+1) :: getAI
       getAI = this%AI
  end function getAI

    !***************************************************
  ! getAJ:
  !     get AJ vector of matrix
  !  
  ! Parameters:
  !     Input, -
  !     Output, getAJ(:)(Integer(ikind))
  !***************************************************
  function getAJ(this)
    implicit none
    class(Sparse), intent(inout)       :: this
    integer(ikind), dimension(this%nnz) :: getAJ
       getAJ = this%AJ
  end function getAJ
  
  !***************************************************
  ! printValue:
  !     prints a single value either on console or a
  !     given filename
  !  
  ! Parameters:
  !     Input, i(int), j(int), filename(char)[opt]
  !     Output, value printed
  !***************************************************
  subroutine printValue(this, i, j, filename)
    implicit none
    integer(ikind), parameter :: fileunit = 90
    class(Sparse), intent(inout) :: this
    integer(ikind), intent(in) :: i
    integer(ikind), intent(in) :: j
    character(*), intent(in), optional :: filename
    if(present(filename)) then
       open(fileunit, file = trim(filename), access = 'append')
       write(fileunit,'(A,I0,A,I0,A,E14.7)') 'Matriz value at row ', i, ' column ', j, ' is ', this%get(i,j)
       close(fileunit)
    else
       write(*,'(A,I0,A,I0,A,E14.7)') 'Matriz value at row ', i, ' column ', j, ' is ', this%get(i,j)
    end if
  end subroutine printValue
  
  !***************************************************
  ! printNonZeros:
  !     Prints all non zeros in a list like format,
  !     either on console or on a given filename
  !  
  ! Parameters:
  !     Input, filename(char)[opt]
  !     Output, non zeros printed
  !***************************************************
  subroutine printNonZeros(this, filename)
    implicit none
    integer(ikind), parameter :: fileunit = 91
    class(Sparse), intent(inout) :: this
    character(*), intent(in), optional :: filename
    integer(ikind) :: i, j
    if(present(filename)) then
       open(fileunit, file = trim(filename), access = 'append')
       write(fileunit,'(A,I0,A)') 'Printing ', this%nnz, ' non zeros'
       write(fileunit,'(A,I0,A,I0)') 'Matrix dimension: ', this%n, 'x', this%n
       write(fileunit,'(A)') '------------------------------------'
       write(fileunit,'(A8,4X,2A12)') 'value', 'row', 'column'
       write(fileunit,'(A)') '------------------------------------'
       do i = 1, this%n
          do j = this%AI(i), this%AI(i+1)-1
             write(fileunit,'(E12.5,I12,I12)')  this%A(j), i, this%AJ(j)
          end do
       end do
       close(fileunit)
       return
    end if
    write(*,'(A,I0,A)') 'Printing ', this%nnz, ' non zeros'
    write(*,'(A,I0,A,I0)') 'Matrix dimension: ', this%n, 'x', this%n
    write(*,'(A)') '------------------------------------'
    write(*,'(A8,4X,2A12)') 'value', 'row', 'column'
    write(*,'(A)') '------------------------------------'
    do i = 1, size(this%AI)-1
       do j = this%AI(i), this%AI(i+1)-1
          write(*,'(E12.5,I12,I12)')  this%A(j), i, this%AJ(j)
       end do
    end do
  end subroutine printNonZeros
  
  !***************************************************
  ! printAll:
  !     Prints the whole matrix, zeros included, on
  !     console or on a filename if given.
  !  
  ! Parameters:
  !     Input, filename(char)[opt]
  !     Output, whole matrix printed
  !***************************************************
  subroutine printAll(this, filename)
    implicit none
    integer(ikind), parameter :: fileunit = 92
    class(Sparse), intent(inout) :: this
    character(*), intent(in), optional :: filename
    integer(ikind) :: i, j
    if(present(filename)) then
       open(fileunit, file = trim(filename), access = 'append')
       write(fileunit, '(/,A,I0,A,I0)') 'Printing Sparse Matrix, size: ', size(this%AI)-1, 'x', size(this%AI)-1 
       do i = 1, this%n
          write(fileunit,'(*(E14.7,2X))') (this%get(i,j),j=1,this%n)
       end do
       close(fileunit)
       return
    end if
    write(*, '(A,I0,A,I0)') 'Printing Sparse Matrix, size: ', this%n, 'x', this%n
    do i = 1, this%n
       write(*,'(*(E14.7,2X))') (this%get(i,j),j=1,this%n)
    end do
  end subroutine printAll
  
  !***************************************************
  ! deleteRowAndCol:
  !     Deletes a given row and column.
  !  
  ! Parameters:
  !     Input, row(int), col(int)
  !     Output, -
  !***************************************************
  subroutine deleteRowAndCol(this, row, col)
    Implicit none
    class(Sparse), intent(inout) :: this
    integer(ikind), intent(in) :: row
    integer(ikind), intent(in) :: col
    integer(ikind) :: i, j, k
    integer(ikind) :: rowSize
    integer(ikind), dimension(:), allocatable :: AI
    if(this%isCRSDone) then
       allocate(AI(size(this%AI)))
       do i = 1, size(AI)
          AI(i) = this%AI(i)
       end do
       deallocate(this%AI)
       do i = size(AI)-1, 1, -1
          if(i == row) then
             rowSize = AI(i+1)-AI(i)
             k = AI(i)
             do while(k < this%nnz-rowSize+1)
                this%AJ(k) = this%AJ(k+rowSize)
                this%A(k) = this%A(k+rowSize)
                k = k + 1
             end do
             this%nnz = this%nnz - rowSize
             do k = row, size(AI)-1
                AI(k) = AI(k+1) - rowSize
             end do
          else 
             do j = AI(i), AI(i+1)-1
                if(this%AJ(j) == col) then
                   k = j
                   do while(k < this%nnz)
                      this%A(k) = this%A(k+1)
                      this%AJ(k) = this%AJ(k+1)
                      k = k + 1
                   end do
                   do k = i+1, size(AI)
                      AI(k) = AI(k)-1
                   end do
                   this%nnz = this%nnz - 1
                end if
             end do
          end if
       end do
       do i = 1, this%nnz
          if(this%AJ(i) > col) then
             this%AJ(i) = this%AJ(i)-1
          end if
       end do
       allocate(this%AI(size(AI)-1))
       do i = 1, size(this%AI)
          this%AI(i) = AI(i)
       end do
    else
       i = 1
       do while(i < this%counter)
          if(this%triplet%row(i) == row .or. this%triplet%col(i) == col) then
             this%triplet%row(i) = this%triplet%row(this%counter)
             this%triplet%col(i) = this%triplet%col(this%counter)
             this%triplet%A(i) = this%triplet%A(this%counter)
             this%counter = this%counter - 1
          else
             i = i + 1
          end if
       end do
       if(this%triplet%row(i) == row .or. this%triplet%col(i) == col) then
          this%counter = this%counter - 1
       end if
       do i = 1, this%counter
          if(this%triplet%row(i) > row) then
             this%triplet%row(i) = this%triplet%row(i) - 1
          end if
          if(this%triplet%col(i) > col) then
             this%triplet%col(i) = this%triplet%col(i) - 1
          end if
       end do
    end if
    this%n = this%n - 1
  end subroutine deleteRowAndCol
  
  !***************************************************
  ! free:
  !     Clears memory space taken by the sparse matrix
  !  
  ! Parameters:
  !     Input, -
  !     Output, -
  !***************************************************
  subroutine free(this)
    implicit none
    class(Sparse), intent(inout) :: this
    if(allocated(this%A)) deallocate(this%A)
    if(allocated(this%AJ)) deallocate(this%AJ)
    if(allocated(this%AI)) deallocate(this%AI)
  end subroutine free


  !***********************************************
  !*              MODULE PROCEDURES              *
  !***********************************************

  !***************************************************
  ! sparse_sparse_prod:
  !     performs the product between two given sparse
  !     matrices. Operator: (*).
  !  
  ! Parameters:
  !     Input, a(Sparse), b(Sparse)
  !     Output, c(Sparse)
  !***************************************************
  function sparse_sparse_prod(a, b) result(c)
    implicit none
    class(Sparse), intent(in) :: a
    class(Sparse), intent(in) :: b
    type(Sparse) :: c
    type(Sparse) :: bTranspose
    real(rkind) :: Cij
    integer(ikind) :: i, j, k
    integer(ikind) :: nnz
    logical :: nnzFound
    if(a%n /= b%n) then
       print'(A)', '** diferent sizes in input sparse matrices! **'
       return
    end if
    bTranspose = transpose(b)
    !find ammount of nnz
    nnz = 0
    do i = 1, a%n
       do j = 1, bTranspose%n
          nnzFound = .false.
          do k = a%AI(i), a%AI(i+1)-1
             do l = bTranspose%AI(j), bTranspose%AI(j+1)-1
                if(a%AJ(k) == bTranspose%AJ(l)) nnzFound = .true.
             end do
          end do
          if(nnzFound) nnz = nnz + 1
       end do
    end do
    c = sparse(nnz = nnz, rows = a%n)
    do i = 1, a%n
       do j = 1, bTranspose%n
          Cij = 0
          do k = a%AI(i), a%AI(i+1)-1
             do l = bTranspose%AI(j), bTranspose%AI(j+1)-1
                if(a%AJ(k) == bTranspose%AJ(l)) then
                   Cij = Cij + a%A(k)*bTranspose%A(l)
                end if
             end do
          end do
          if(abs(Cij) > 1d-12) call c%append(Cij, i, j)
       end do
    end do
    call bTranspose%free()
    call c%makeCRS(.false.)
  end function sparse_sparse_prod
  
  !***************************************************
  ! sparse_vect_prod(*):
  !      Performs the product between a sparse matrix
  !      and a condensed real vector.
  !      Operator: (*).
  !  
  ! Parameters:
  !     Input, mat(Sparse), vect(realrkind)
  !     Output, res(realrkind)
  !***************************************************
  function sparse_vect_prod(mat, vect) result(res)
    implicit none
    class(Sparse), intent(in) :: mat
    real(rkind), dimension(:), intent(in) :: vect
    real(rkind), dimension(size(vect)) :: res
    real(rkind) :: val
    integer(ikind) :: i, k
    do i = 1, mat%n
       val = 0.0_rkind
       do k = mat%AI(i), mat%AI(i+1)-1
          val = val + mat%A(k)*vect(mat%AJ(k))
       end do
       res(i) = val
    end do
  end function sparse_vect_prod
  
  !***************************************************
  ! coef_sparse_prod(*):
  !      Performs the product between a sparse matrix
  !      and a coeficient real vector.
  !      Operator: (*).
  !  
  ! Parameters:
  !     Input, mat(Sparse), coef(realrkind)
  !     Output, res(realrkind)
  !***************************************************
  function coef_sparse_prod(coef ,mat) result(res)
    implicit none
    type(Sparse), intent(in) :: mat
    type(Sparse) :: res
    real(rkind), intent(in) :: coef
    real(rkind) :: c
    integer(ikind) :: i
    c = coef
    res = mat
    do i = 1, res%nnz
       res%A(i) = res%A(i)*c
    end do
  end function coef_sparse_prod
  
  !***************************************************
  ! sparse_sparse_add:
  !     performs the addition of sparse a plus
  !     sparse b. Operator: (+).
  !  
  ! Parameters:
  !     Input, a(Sparse), b(Sparse)
  !     Output, c(Sparse)
  !***************************************************
  function sparse_sparse_add(a, b) result(c)
    implicit none
    class(Sparse), intent(in) :: a
    class(Sparse), intent(in) :: b
    type(Sparse) :: c
    integer(ikind) :: counter, rowSize, i, k
    c = sparse(nnz = a%nnz+b%nnz, rows = a%n)
    counter = 1
    do i = 1, a%n
       rowSize = a%AI(i+1) - a%AI(i)
       do k = counter, counter+rowSize-1
          call c%append(a%A(k), i, a%AJ(k))
       end do
       counter = counter + rowSize
    end do
    counter = 1
    do i = 1, b%n
       rowSize = b%AI(i+1) - b%AI(i)
       do k = counter, counter+rowSize-1
          call c%append(b%A(k), i, b%AJ(k))
       end do
       counter = counter + rowSize
    end do
    call c%makeCRS
  end function sparse_sparse_add
  
  !***************************************************
  ! sparse_sparse_sub:
  !     performs the subtraction of sparse a and
  !     sparse b. Operator: (-).
  !  
  ! Parameters:
  !     Input, a(Sparse), b(Sparse)
  !     Output, c(Sparse)
  !***************************************************
  function sparse_sparse_sub(a, b) result(c)
    implicit none
    class(Sparse), intent(in) :: a
    class(Sparse), intent(in) :: b
    type(Sparse) :: c
    integer(ikind) :: counter, rowSize, i, k
    c = sparse(nnz = a%nnz+b%nnz, rows = a%n)
    counter = 1
    do i = 1, a%n
       rowSize = a%AI(i+1) - a%AI(i)
       do k = counter, counter+rowSize-1
          call c%append(a%A(k), i, a%AJ(k))
       end do
       counter = counter + rowSize
    end do
    counter = 1
    do i = 1, b%n
       rowSize = b%AI(i+1) - b%AI(i)
       do k = counter, counter+rowSize-1
          call c%append(-1.0_rkind*b%A(k), i, b%AJ(k))
       end do
       counter = counter + rowSize
    end do
    call c%makeCRS
  end function sparse_sparse_sub
  
  !***************************************************
  ! transpose:
  !     Obtains the transpose of sparse matrix a
  !  
  ! Parameters:
  !     Input, a(Sparse)
  !     Output, b(Sparse)
  !***************************************************
  function transpose(a) result(b)
    implicit none
    class(Sparse), intent(in) :: a
    type(Sparse) :: b
    integer(ikind), dimension(a%n) :: colCounter
    integer(ikind) :: i, j, k, counter
    b = sparse(nnz = a%nnz, rows = a%n)
    colCounter = 0
    do i = 1, b%nnz
       colCounter(a%AJ(i)) = colCounter(a%AJ(i)) + 1
    end do
    allocate(b%AI(b%n+1))
    allocate(b%A(b%nnz))
    allocate(b%AJ(b%nnz))
    b%AI = 1
    do i = 2, b%n+1
       b%AI(i) = b%AI(i-1) + colCounter(i-1)
    end do
    counter = 1
    colCounter = 0
    do i = 1, b%n
       do k = a%AI(i), a%AI(i+1)-1
          j = a%AJ(k)
          b%AJ(b%AI(j)+colCounter(j)) = i
          b%A(b%AI(j)+colCounter(j)) = a%A(counter)
          colCounter(j) = colCounter(j) + 1
          counter = counter + 1
       end do
    end do
  end function transpose
  
  !***************************************************
  ! norm:
  !     Computes the frobenius-norm of a sparse matrix
  !     https://en.wikipedia.org/wiki/Matrix_norm#Frobenius_norm
  !  
  ! Parameters:
  !     Input, a(Sparse)
  !     Output, norm(Realrkind)
  !***************************************************
  real(rkind) function norm(a)
    implicit none
    class(Sparse), intent(in) :: a
    integer(ikind) :: i, j, counter
    norm = 0
    counter = 1
    do while(counter <= a%nnz)
       norm = norm + a%A(counter)*a%A(counter)
       counter = counter + 1
    end do
    norm = sqrt(norm)
  end function norm
  
  !***************************************************
  ! trace:
  !     Computes the sum of the elements of the
  !     diagonal of a sparse matrix
  !  
  ! Parameters:
  !     Input, a(Sparse)
  !     Output, trace(Realrkind)
  !***************************************************
  real(rkind) function trace(a)
    implicit none
    class(Sparse), intent(in) :: a
    integer(ikind) :: i, j
    trace = 0
    do i = 1, a%n
       do j = a%AI(i), a%AI(i+1)-1
          if(a%AJ(j) == i) then
             trace = trace + a%A(j)
             exit
          end if
       end do
    end do
  end function trace
  
  !***************************************************
  ! id:
  !     Computes the identity matrix of order n
  !  
  ! Parameters:
  !     Input, n(integer(ikind))
  !     Output, id(Sparse)
  !***************************************************
  function id(n) result(a)
    implicit none
    type(Sparse) :: a
    integer(ikind), intent (in) :: n
    integer(ikind) :: i
    a = sparse(nnz = n, rows = n)
    do i = 1, n
       call a%append(1.0_rkind, i, i)
    end do
    call a%makeCRS
  end function id

  type(Sparse) function inverse(A)
    implicit none
    class(Sparse)                          , intent(inout) :: A
    real(rkind)             , dimension(:) , allocatable   :: vector
    real(rkind)             , dimension(:) , allocatable   :: solution
    type(mkl_pardiso_handle), dimension(64)                :: pt
    real(rkind)             , dimension(1)                 :: ddum
    integer(ikind)                                         :: maxfct
    integer(ikind)                                         :: mnum
    integer(ikind)                                         :: mtype
    integer(ikind)                                         :: phase
    integer(ikind)                                         :: nrhs
    integer(ikind)          , dimension(64)                :: iparm(64)
    integer(ikind)                                         :: msglvl
    integer(ikind)                                         :: error
    integer(ikind)          , dimension(1)                 :: idum
    integer(ikind)                                         :: i, j
    logical                                    :: sortRows = .false.
    allocate(vector(A%getn()), solution(A%getn()))
    inverse = Sparse(A%getn()**2, A%getn())
    pt(1:64)%DUMMY = 0.d0
    maxfct        = 1
    mnum          = 1
    mtype         = 1      ! real and structurally symmetric 
    phase         = 13     ! analisys, numerical factorization, solve,
                           ! iterative refinement
    idum          = 0
    nrhs          = 1
    iparm         = 0
    iparm(1)      = 1      ! user defined iparms
    iparm(2)      = 2      ! 3 The parallel (OpenMP) version of the
                           !nested dissection algorithm.
                           ! 2 The nested dissection algorithm from
                           !the METIS package.
                           ! 0 The minimum degree algorithm.
    iparm(4)      = 61     ! LU-preconditioned CGS iteration with a
                           ! stopping criterion of
                           ! 1.0E-6 for nonsymmetric matrices.
    iparm(24)     = 1      ! two-level factorization algorithm.
    iparm(60)     = 1      ! in-core mode or out-core mode
    msglvl        = 0      ! non-print statistical information.
    error         = 0
    do i = 1, A%getn()
       vector    = 0.d0
       vector(i) = 1.d0
       solution  = vector
       call pardiso (pt, maxfct, mnum, mtype, phase, A%getn()  &
            , A%geta(), A%getai(), A%getaj(), idum, nrhs, iparm &
            , msglvl, vector, solution, error                  )
       if (error /= 0) then
          write(*,'(a,i5)') 'Inverse matrix error'
          stop
       end if
       do j = 1, A%getn()
          if (solution(j) .ne. 0.d0) then
             call inverse%append( val = solution(j) , row = i, col = j)
          end if
       end do
       write(*,*) 'Inverse =>', (100*i/A%getn()),'%'
    end do
    deallocate(vector,solution)
    call inverse%makeCRS(sortRows)
    print'(a)', 'Inverse ok'
    return
  end function inverse

  type(Sparse) function inverseLumped(matrix)
    implicit none
    class(Sparse), intent(inout) :: matrix
    logical                      :: sortRows = .false.
    integer(ikind) :: i
    inverseLumped = Sparse(nnz = matrix%getn(), rows = matrix%getn())
    do i = 1, matrix%getn()
       call inverseLumped%append(1._rkind/matrix%get(i,i),i,i)
    end do
    call inverseLumped%makeCRS(sortRows)
  end function inverseLumped
  
end module SparseKit

