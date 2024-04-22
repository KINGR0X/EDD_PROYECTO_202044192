module matriz_dis
    implicit none
    private

    type :: node_val
        private
        logical :: exists = .false.
        integer, allocatable :: id
        character(len=:), allocatable :: color

    end type node_val

    type :: node
        private
        integer :: i, j
        character(len=:), allocatable :: color
        integer, allocatable :: id
        type(node), pointer :: up => null()
        type(node), pointer :: down => null()
        type(node), pointer :: right => null()
        type(node), pointer :: left => null()
    end type node

    type, public :: matrix
        private
        type(node), pointer :: root => null()
        integer :: width = 0
        integer :: height = 0
    contains
        procedure :: insert
        procedure :: insertRowHeader
        procedure :: insertColumnHeader
        procedure :: insertInRow
        procedure :: insertInColumn
        procedure :: searchRow
        procedure :: searchColumn
        procedure :: nodeExists
        procedure :: print
        procedure :: graficarCapas
        procedure :: printColumnHeaders
        procedure :: getValue
    end type

contains

    subroutine insert(self, i, j, color, id) 
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: i
        integer, intent(in) :: j
        integer, intent(in) :: id
        character(len=*), intent(in) :: color

        type(node), pointer :: new
        type(node), pointer :: row
        type(node), pointer :: column

        allocate(new)
        new = node(i=i, j=j, color=color, id=id)

        if(.not. associated(self%root)) then
            allocate(self%root)
            self%root = node(i=-1, j=-1)
        end if

        row => self%searchRow(i)
        column => self%searchColumn(j)

        if(j > self%width) self%width = j
        if(i > self%height) self%height = i

        if(.not. self%nodeExists(new)) then
            if(.not. associated(column)) then
                column => self%insertColumnHeader(j)
            end if

            if(.not. associated(row)) then
                row => self%insertRowHeader(i)
            end if
            call self%insertInColumn(new, row)
            call self%insertInRow(new, column)
        end if
    end subroutine insert

    function searchColumn(self, j) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: j

        type(node), pointer :: actual
        actual => self%root

        do while(associated(actual))
            if(actual%j == j) return
            actual => actual%right
        end do
    end function searchColumn

    function searchRow(self, i) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i

        type(node), pointer :: actual
        actual => self%root

        do while(associated(actual))
            if(actual%i == i) return
            actual => actual%down
        end do
    end function searchRow

    function nodeExists(self, new) result(exists)
        class(matrix), intent(inout) :: self  
        type(node), pointer :: new
        
        logical :: exists
        type(node), pointer :: rowHeader
        type(node), pointer :: column
        rowHeader => self%root
        exists = .false.

        do while(associated(rowHeader))
            if(rowHeader%i == new%i) then
                column => rowHeader
                do while(associated(column))
                    if(column%j == new%j) then
                        column%color = new%color
                        exists = .true.
                        return
                    end if
                    column => column%right
                end do
                return
            end if
            rowHeader => rowHeader%down
        end do
        return
    end function nodeExists

    function insertRowHeader(self, i) result(newRowHeader)
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: i

        type(node), pointer :: newRowHeader
        allocate(newRowHeader)

        newRowHeader = node(i=i, j=-1)
        call self%insertInRow(newRowHeader, self%root)
    end function insertRowHeader

    subroutine insertInRow(self, new, rowHeader)
        class(matrix), intent(inout) :: self
        type(node), pointer :: new
        type(node), pointer :: rowHeader

        type(node), pointer :: actual
        actual => rowHeader

        do while(associated(actual%down))
            if(new%i < actual%down%i .and. new%i > actual%i) then
                new%down => actual%down
                new%up => actual
                actual%down%up => new
                actual%down => new
                exit
            end if
            actual => actual%down
        end do

        if(.not. associated(actual%down)) then
            actual%down => new
            new%up => actual
        end if
    end subroutine insertInRow

    function insertColumnHeader(self, j) result(newColumnHeader)
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: j

        type(node), pointer :: newColumnHeader
        allocate(newColumnHeader)

        newColumnHeader = node(i=-1, j=j)
        call self%insertInColumn(newColumnHeader, self%root)
    end function insertColumnHeader

    subroutine insertInColumn(self, new, columnHeader)
        class(matrix), intent(inout) :: self
        type(node), pointer :: new
        type(node), pointer :: columnHeader
        
        type(node), pointer :: actual
        actual => columnHeader
        do while(associated(actual%right))
            if(new%j < actual%right%j .and. new%j > actual%j) then
                new%right => actual%right
                new%left => actual
                actual%right%left => new
                actual%right => new
                exit
            end if
            actual => actual%right
        end do
        
        if(.not. associated(actual%right)) then
            actual%right => new
            new%left => actual
        end if
    end subroutine insertInColumn

    subroutine print(self)
        class(matrix), intent(inout) :: self  
        integer :: i
        integer :: j
        type(node), pointer :: aux
        type(node_val) :: val
        aux => self%root%down

        call self%printColumnHeaders()

        do i = 0, self%height
            print *, ""
            write(*, fmt='(I3)', advance='no') i
            do j = 0, self%width
                val = self%getValue(i,j)
                if(.not. val%exists) then
                    write(*, fmt='(I3)', advance='no') 0
                else
                    write(*, fmt='(L3)', advance='no') val%color

                    !write(*, fmt='(A)', advance='no') val%color

                end if
            end do
        end do
    end subroutine print

    subroutine printColumnHeaders(self)
        class(matrix), intent(in) :: self
        integer :: j

        do j=-1, self%width
            write(*, fmt='(I3)', advance='no') j
        end do
    end subroutine printColumnHeaders

    function getValue(self, i, j) result(val)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j
        
        type(node), pointer :: rowHeader
        type(node), pointer :: column
        type(node_val) :: val
        rowHeader => self%root

        do while(associated(rowHeader))
            if(rowHeader%i == i) then
                column => rowHeader
                do while(associated(column))
                    if(column%j == j) then
                        val%color = column%color
                        val%id = column%id
                        val%exists = .true.
                        return
                    end if
                    column => column%right
                end do
                return
            end if
            rowHeader => rowHeader%down
        end do
    end function getValue

    subroutine graficarCapas(self,filename, cap,title,size_font)
        class(matrix), intent(inout) :: self  
        character(len=*), intent(in) :: filename, title
        ! integer, intent(in) :: tam
        integer :: fila,f,size_font
        integer :: colum,c
        integer :: i
        type(node), pointer :: aux
        type(node_val) :: nodeVal
        integer :: unit
        integer, dimension(:), intent(in) :: cap
        logical :: coincidencia
        
        if(.not. associated(self%root)) then
            return
        end if

        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph Matriz {'
        write(unit, *) 'label= ', '"',title, cap,'"',';'
        write(unit, *) 'fontsize=',size_font,';'
        write(unit, *) '    node [shape=box, style=filled];'

        aux => self%root%down
        do fila=-1, self%width
            if (fila == -1) then
                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0, '" [label="root"]'
                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0,'" -> "Node_fila_-1_0','"' 
                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0,'" -> "Node_fila_-1_0','"[dir=back]' 
                
            else
                write(unit, '(*(g0))') '"Node_encabezado_', fila-1, '_', 0,'" -> "Node_encabezado_', fila, '_', 0, '"'
                write(unit, '(*(g0))') '"Node_encabezado_', fila-1, '_', 0,'" -> "Node_encabezado_', fila, '_', 0, '"[dir=back]' 

                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0, '" [label="',fila,'"]'
                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0,'" -> "Node', 0,'_',fila,'"'
                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0,'" -> "Node', 0,'_',fila,'"[dir=back]'



            end if
        end do

        write(unit, '(*(g0))') ' '
        write(unit, '(*(g0))', advance="no") '{rank=same;'
        do fila=-1, self%width
            write(unit, '(*(g0))',advance="no") '"Node_encabezado_', fila, '_', 0, '";'
        end do
        write(unit, '(*(g0))',advance="yes") '};'

        fila=0
        f=0

        do fila = 0, self%height
            f=fila
            colum = 0
            write(unit, '(*(g0))') '"Node_fila_', fila-1,"_",colum,'" [label="',fila,'"]'

            write(unit, '(*(g0))') '"Node_fila_', fila-1,"_",colum,'" -> "Node', fila,'_',colum,'"'
            write(unit, '(*(g0))') '"Node_fila_', fila-1,"_",colum,'" -> "Node', fila,'_',colum,'"[dir=back]'

            if(.not. fila== self%height) then
                write(unit, '(*(g0))') '"Node_fila_', fila-1,"_",colum,'" -> "Node_fila_', fila,'_',colum,'"'
                write(unit, '(*(g0))') '"Node_fila_', fila-1,"_",colum,'" -> "Node_fila_', fila,'_',colum,'"[dir=back]'
            end if

            
            do colum = 0, self%width
                coincidencia = .false.
                nodeVal = self%getValue(fila,colum)

                if(nodeVal%exists) then

                    do i=1, size(cap)
                        if(nodeVal%id == cap(i)) then
                            coincidencia = .true.
                            exit
                        end if
                    end do

                    if(coincidencia) then
                        write(unit, '(*(g0))') '"Node', fila,"_",colum,'" [label= " ", fillcolor="',nodeVal%color, &
                        '", group=', fila,']'
                    else
                        write(unit, '(*(g0))') '"Node', fila,"_",colum,'" [label="',colum,'", group=', fila,']'
                        write(unit, '(*(g0))') '"Node', fila,"_",colum,'" [label="", group=', fila,']'
                    end if
                
                else
                    write(unit, '(*(g0))') '"Node', fila,"_",colum,'" [label="',colum,'", group=', fila,']'
                    write(unit, '(*(g0))') '"Node', fila,"_",colum,'" [label="", group=', fila,']'
                end if

                if (.not. colum==0) then
                    write(unit, '(*(g0))') '"Node', fila,"_",colum-1,'" -> "Node', fila,'_',colum,'"'
                    write(unit, '(*(g0))') '"Node', fila,"_",colum-1,'" -> "Node', fila,'_',colum,'"[dir=back]'
                end if

                if (.not. colum==0 ) then

                    if (.not. fila==self%height) then
                        write(unit, '(*(g0))') '"Node', fila,"_",colum-1,'" -> "Node', fila+1,'_',colum-1,'"'
                        write(unit, '(*(g0))') '"Node', fila,"_",colum-1,'" -> "Node', fila+1,'_',colum-1,'"[dir=back]'
                    end if
                end if

                if (colum==self%width) then
                    if (.not. fila==self%height) then
                        write(unit, '(*(g0))') '"Node', fila,"_",colum,'" -> "Node', fila+1,'_',colum,'"'
                        write(unit, '(*(g0))') '"Node', fila,"_",colum,'" -> "Node', fila+1,'_',colum,'"[dir=back]'
                    end if
                end if

            end do
            write(unit, '(*(g0))') ' '
            write(unit, '(*(g0))', advance="no") '{rank=same;'
            c=0
            do c=-1, self%width
                if (c==-1)then 
                    write(unit, '(*(g0))',advance="no") '"Node_fila_', f-1, '_', c+1, '";'
                else
                    write(unit, '(*(g0))',advance="no") '"Node', f,"_",c,'";'
                end if
            end do
            write(unit, '(*(g0))',advance="yes") '};'

            write(unit, '(*(g0))') ' '
        end do

        ! write(unit, *) 'label= ', '"',title, cap,'"',';'
        ! write(unit, *) 'fontsize=400;'
        write(unit, *) '}'
        close(unit)
    
        call system('dot -Gnslimit=2 -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'

    end subroutine graficarCapas
    
end module matriz_dis

! program main
!     use matrix_m
!     implicit none
    
!     type(matrix) :: m
!     !             i, j
!     !            fila, columna
!     call m%insert(1, 5, "#000001")
!     call m%insert(0, 3, "#FF0000")  
!     call m%insert(1, 1, "#000000")
!     call m%insert(4, 1, "#000041")
!     call m%insert(5, 3, "#000000")
!     call m%insert(5, 10, "#000000")
!     call m%insert(1, 1, "#000000")
!     call m%insert(3, 1, "#000000")


!     call m%print()

!     call m%graficar("capas.dot")



! end program main