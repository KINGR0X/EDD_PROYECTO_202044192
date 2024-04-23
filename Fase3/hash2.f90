module hash_module
    implicit none
    private
    public :: insert, print, inicial, finalize

    integer(kind=16), dimension(:), allocatable :: table
    integer :: SIZE
    integer :: contador_colisiones
    integer :: total_ocupado ! Para redimensionar la tabla al llegar a un 70% de uso

    type :: Registro
        integer(kind=16) :: numero
        character(len=60) :: nombre
        character(len=60) :: apellido
        character(len=60) :: genero
        character(len=60) :: direccion
        integer(kind=16) :: telefono
    end type Registro

    type(Registro), dimension(:), allocatable :: registros

contains

    subroutine inicial(initial_size)
        integer, intent(in) :: initial_size
        SIZE = initial_size
        allocate(table(0:SIZE))
        table = 0
        allocate(registros(0:SIZE))
        contador_colisiones = 0
        total_ocupado = 0 
    end subroutine inicial

    subroutine finalize()
        deallocate(table, registros)
    end subroutine finalize
    
    subroutine insert(key, nombre, apellido, genero, direccion, telefono)
        integer(kind=16), intent(in) :: key
        character(len=60), intent(in) :: nombre, apellido, genero, direccion
        integer(kind=16), intent(in) :: telefono

        integer(kind=16) :: index, hash

        integer(kind=16), dimension(:), allocatable :: new_table
        integer :: i
        integer :: bool_insertado

        bool_insertado = 0

        ! Rehash si la tabla está ocupada al 70%
        if (total_ocupado >= (SIZE+1) * 0.7) then
            SIZE = (SIZE+1) * 2
            SIZE= SIZE - 1
            allocate(new_table(0:SIZE))
            new_table = 0

            ! Copiar los valores de la tabla existente a la nueva tabla
            do i = 0, SIZE/2
                new_table(i) = table(i)
            end do

            ! Liberar la memoria de la tabla existente
            deallocate(table)

            ! Asignar la nueva tabla
            allocate(table(0:SIZE))
            table = new_table
            print *, ' '
            print *, 'Nueva tabla de tamaño ', SIZE
        end if

        ! Calcular el índice usando la función de hash
        hash = modulo(key, SIZE+1)

        ! Reorganizar elementos si hay colisión
        ! Si la posición está vacía, insertar el elemento
        if (table(hash) == 0) then

            registros(hash)%numero = key
            registros(hash)%nombre = nombre
            registros(hash)%apellido = apellido
            registros(hash)%genero = genero
            registros(hash)%direccion = direccion
            registros(hash)%telefono = telefono

            table(hash) = 1 ! Marcar la posición como ocupada

            total_ocupado = total_ocupado + 1
            print *, 'Insertando ', nombre, apellido, ' con número ', key, ' en la posición ', hash
        else

            ! se ejecuta mientras insertado sea 0
            do while (bool_insertado == 0)
                print *, 'XXX Contador colisiones= ', contador_colisiones

                index = modulo(((modulo(key, 8)) * contador_colisiones), SIZE+1)
                
                if (table(index) == 0) then
                    registros(index)%numero = key
                    registros(index)%nombre = nombre
                    registros(index)%apellido = apellido
                    registros(index)%genero = genero
                    registros(index)%direccion = direccion
                    registros(index)%telefono = telefono

                    table(index) = 1 ! Marcar la posición como ocupada

                    bool_insertado = 1
                    contador_colisiones = contador_colisiones + 1
                    exit
                end if

                contador_colisiones = contador_colisiones + 1
            end do

            total_ocupado = total_ocupado + 1
            print *, 'Colisión en ', nombre, apellido, ' con número ', key, ' insertando en ', index
        end if
    end subroutine insert

    subroutine print
        integer :: i
        print *, ' '
        print * , 'Tabla hash:'
        do i = 0, SIZE
            if (table(i) == 1) then
                print *, 'Posicion ', i, ': Numero: ', registros(i)%numero

                print *, "                        Nombre: ", trim(registros(i)%nombre)
                print *, "                        Apellidos: ", trim(registros(i)%apellido)
                print *, "                        Genero: ", trim(registros(i)%genero)
                print *, "                        Direccion: ", trim(registros(i)%direccion)
                print *, "                        Telefono: ", registros(i)%telefono
                
            else
                print *, 'Posicion ', i, ': Vacio'
            end if
        end do

        print *, 'Total ocupado: ', total_ocupado
    end subroutine print

end module hash_module

program main
    use hash_module
    implicit none

    integer(kind=16) :: numero_grande, telefono
    character(len=60) :: nombre, apellido, genero, direccion
    
    call inicial(6)

    nombre = "Juan Pablo"
    apellido = "Gonzalez Carrillo"
    genero = "Masculino"
    direccion = "8 calle 3-45 Zona 1"
    telefono = 12345678
    call insert(3746582726365_16, nombre, apellido, genero, direccion, telefono)

    nombre = 'Maria Jose'
    apellido = 'Portillo Velazquez'
    genero = "Femenino"
    direccion = "6 calle 8-40 Villa Nueva"
    telefono = 12345677
    call insert(1234567891011_16, nombre, apellido, genero, direccion, telefono)  ! posición 6,  no se usa rehash

    nombre = 'Pedro'
    apellido = 'Lopez'
    genero = "Femenino"
    direccion = "6 calle 8-40 Villa Nueva"
    telefono = 12345677
    call insert(00000000000013_16, nombre, apellido, genero, direccion, telefono)  ! posición 0,  contador_colisiones = 0

    nombre = 'Ana'
    apellido = 'Martinez'
    genero = "Femenino"
    direccion = "6 calle 8-40 Villa Nueva"
    telefono = 12345677
    call insert(00000000000013_16, nombre, apellido, genero, direccion, telefono)  ! posición 5,  contador_colisiones = 1

    nombre = 'Numero trece'
    apellido = 'Martinez'
    genero = "Femenino"
    direccion = "6 calle 8-40 Villa Nueva"
    telefono = 12345677
    call insert(00000000000013_16, nombre, apellido, genero, direccion, telefono)  ! posición 6,  contador_colisiones = 1

    ! nombre = 'Laura'
    ! apellido = 'Sanchez'
    ! genero = "Femenino"
    ! direccion = "6 calle 8-40 Villa Nueva"
    ! telefono = 12345677
    ! call insert(9921354345345_16, nombre, apellido, genero, direccion, telefono)

    ! nombre = 'Carlos'
    ! apellido = 'Ruiz'
    ! genero = "Femenino"
    ! direccion = "6 calle 8-40 Villa Nueva"
    ! telefono = 12345677
    ! call insert(7621354345345_16, nombre, apellido, genero, direccion, telefono)

    ! nombre = 'Carlos 2'
    ! apellido = 'Ruiz'
    ! genero = "Masculino"
    ! direccion = "6 calle 8-40 Villa Nueva"
    ! telefono = 12345677
    ! call insert(7621354345346_16, nombre, apellido, genero, direccion, telefono)

    call print()

    call finalize()
end program main
