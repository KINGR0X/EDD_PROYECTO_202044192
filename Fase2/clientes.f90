module clientes_f2
    implicit none
    private

    type, public :: node
        private
        character(:), allocatable :: value
        character(:), allocatable :: password
        character(:), allocatable :: dpi
        type(node), pointer :: next     
    end type node

    type, public :: cola
        private
        type(node), pointer :: head => null()
        type(node), pointer :: end => null()
    contains
        procedure :: append
        procedure :: delete
        procedure :: print
        procedure :: return_cliente_name
        procedure :: graficar
        procedure :: modificar_dpi
    end type cola

contains

    subroutine modificar_dpi(this, dpi, nuevo_nombre, nuevo_password)
        class(cola), intent(inout) :: this
        character(len=*), intent(in) :: dpi
        character(len=*), intent(in) :: nuevo_nombre
        character(len=*), intent(in) :: nuevo_password

        type(node), pointer :: current

        current => this%head

        do while (associated(current))
            if (trim(current%dpi) == trim(dpi)) then
                ! Se encontró el cliente que coincide con el DPI proporcionado
                current%value = nuevo_nombre
                current%password = nuevo_password
                print *, 'Datos del cliente con DPI', dpi, 'modificados correctamente:'
                print *, "Nuevo nombre:", current%value
                print *, "Nuevo password:", current%password
                return
            end if
            current => current%next
        end do

        ! Si llega aquí, significa que no se encontró el cliente en la cola
        print *, "No se encontró ningún cliente con el DPI", dpi
    end subroutine modificar_dpi



    subroutine append(this, value, dpi, password)
        class(cola), intent(inout) :: this
        character(len=*), intent(in) :: value
        character(len=*), intent(in) :: password
        character(len=*), intent(in) :: dpi

        type(node), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%password = password
        temp%dpi = dpi
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
            this%end => temp
        else
            this%end%next => temp
            this%end => temp
        end if
    end subroutine append


    subroutine delete(this,value)
        class(cola), intent(inout) :: this
        character(len=*), intent(in) :: value

        type(node), pointer :: current, previous

        if (.not. associated(this%head)) then
            ! La cola está vacía
            print *, 'La cola de recepción está vacía'
            return
        end if

        current => this%head
        previous => null()

        do while (associated(current))
            if (trim(current%value) == trim(value)) then
                ! Se encontró el cliente que coincide con el nombre proporcionado
                print *, current%value, ' fue eliminado'
                if (associated(previous)) then
                    ! El nodo a eliminar no es el primero
                    previous%next => current%next
                else
                    ! El nodo a eliminar es el primero
                    this%head => current%next
                end if
                deallocate(current)
                return
            end if
            previous => current
            current => current%next
        end do

        ! Si llega aquí, significa que no se encontró el cliente en la cola
        print *, "no se econtro", value

    end subroutine delete

    subroutine print(this)
        class(cola), intent(in) :: this
        type(node), pointer :: current

        current => this%head

        print *, '-------- Cola de Recepcion --------'
        print *,  " "

        do while (associated(current))
            print *, current%value
            print *, "password: ", current%password
            print *, "dpi: ", current%dpi
            print *, "---------------"
            current => current%next
        end do 
    end subroutine print

    function return_cliente_name(this) result(head_value)
        class(cola), intent(in) :: this
        character(len=:), allocatable :: head_value

        if (.not. associated(this%head)) then
            ! print *, 'Cola esta vacia'
            head_value = ""
        else
            head_value = this%head%value
        end if
    end function return_cliente_name

    subroutine graficar(this, filename)
        class(cola), intent(in) :: this
        character(len=*), intent(in) :: filename
        type(node), pointer :: current
        character(len=:), allocatable :: dpi, password

        integer :: unit
        integer :: count

        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph clientes_f2 {'
        ! write(unit, *) 'label= "Cola de Recepcion";'
        write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=gold];' ! Aplicar atributos a todos los nodos
        
        ! Escribir nodos y conexiones
        current => this%head
        count = 0

        do while (associated(current))
            count = count + 1
            dpi= current%dpi
            password= current%password
            write(unit, *) '    "Node', count, '" [label="', current%value, "\n DPI =",dpi,"\n password =",password,'"];'
            
            if (associated(current%next)) then
                write(unit, *) '    "Node', count, '" -> "Node', count+1, '";'
            end if
            current => current%next
        end do 

        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)
    
        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
    
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
    end subroutine graficar

end module clientes_f2

! program main
!     use clientes_f2
!     implicit none

!     type(cola) :: cola_r
    
!     !                    value, dpi, password
!     call cola_r%append("AUX EDD","1234567890123","edd1s2024")
!     call cola_r%append("cliente 2","6745567890123","edd1s2022")
!     call cola_r%append("cliente 3","202044192","contra123")

!     ! call cola_r%print()

!     call cola_r%delete("cliente 2")
!     ! call cola_r%delete("cliente 2")

!     call cola_r%modificar_dpi("202044192","Yo merengues","nuevacontra123")

!     call cola_r%print()
!     call cola_r%graficar('clientes_f2')

! end program main