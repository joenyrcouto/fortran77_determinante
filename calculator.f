      program tarefa5 !autor Joenyr ;)

      real v(362880, 10), v2(362880, 10), A(19,19), det/0/,
     & produto/1/
      integer i, j, s, k/200/, nlinha/0/, ncoluna/0/, onoff/0/,
     & fatorial/1/, col_paridade, N
      character*200 linha

c     Ler permutação base
      open(UNIT=10, FILE='permutacoes.txt')

      !Contar linha
      do
        read(10, *, end=20)
        nlinha = nlinha+1
      end do

      !Salvar string
20    rewind(10)
      read(10, '(A)') linha

      !Contar colunas sem espaço
21    if (linha(k:k) .ne. ' ') then
        k = k-1
        if (k .gt. 0) goto 21
      end if

      !Contar qnt de numeros
      do j=1,k
        if (linha(j:j) .ne. ' ') then
          if (onoff .eq. 0) then
            ncoluna = ncoluna+1
            onoff = 1
          end if
        else
          onoff = 0
        end if
      end do

      !Ler numeros
      rewind(10)
      do i=1,nlinha
        read(10,*) (v(i,j), j=1,ncoluna)
      end do
      close(10)

c     Ler matriz alvo

      !Salvar string
      open(UNIT=11, FILE='matriz.txt')
      read(11, '(A)') linha

      k = 200
      !Contar colunas sem espaço
22    if (linha(k:k) .ne. ' ') then
        k = k-1
        if (k .gt. 0) goto 22
      end if

      onoff = 0
      !Contar qnt de numeros
      do j=1,k
        if (linha(j:j) .ne. ' ') then
          if (onoff .eq. 0) then
            N = N+1
            onoff = 1
          end if
        else
          onoff = 0
        end if
      end do

      !Ler numeros
      rewind(11)
      do i=1,N
        read(11,*) (A(i,j), j=1,N)
      end do

c     Gerar permutações

23    if ((ncoluna-1) .lt. N) then

      !Copiar vetores
      do i=1,nlinha
        do s=(i-1)*ncoluna+1,i*ncoluna
          do j=1,ncoluna
            v2(s,j) = v(i,j)
          end do
        end do
      end do

      !Organizar novo vetor
      do i=1,nlinha*ncoluna
        do j=0,mod(i-1,ncoluna)
          v2(i,ncoluna-j+1) = v2(i,ncoluna-j)
        end do
      end do

      !Adicionar número N+1 a matriz e arrumar paridade
      do i=1,nlinha*ncoluna
        v2(i,ncoluna-mod(i-1,ncoluna)) = ncoluna
        v2(i,ncoluna+1) = (-1)**(mod(i+1,2))*v2(i,ncoluna+1)
      end do

      nlinha = nlinha*ncoluna
      ncoluna = ncoluna+1

      end if

c     Gerar permutações de ordens maiores

      if ((ncoluna-1) .lt. N) then

      !Copiar vetores
      do i=1,nlinha
        do s=(i-1)*ncoluna+1,i*ncoluna
          do j=1,ncoluna
            v(s,j) = v2(i,j)
          end do
        end do
      end do

      !Organizar novo vetor
      do i=1,nlinha*ncoluna
        do j=0,mod(i-1,ncoluna)
          v(i,ncoluna-j+1) = v(i,ncoluna-j)
        end do
      end do

      !Adicionar número N+1 a matriz e arrumar paridade
      do i=1,nlinha*ncoluna
        v(i,ncoluna-mod(i-1,ncoluna)) = ncoluna
        v(i,ncoluna+1) = (-1)**(mod(ncoluna-mod(i-1,ncoluna)+1,2))*
     & v(i,ncoluna+1)
      end do

      nlinha = nlinha*ncoluna
      ncoluna = ncoluna+1

      goto 23
      end if

c     Imprimir determinante

      col_paridade = ncoluna
      do i = 1, nlinha
          !Calcula o produto a_{1,σ(1)} * a_{2,σ(2)} * ... * a_{N,σ(N)}
          produto = 1.0
          do s = 1, N
              produto = produto * A(s, int(v2(i,s)))
          end do

          !Adiciona ao determinante com a paridade
          if (mod(N,2) .eq. 0) then
            det = det + v2(i, col_paridade) * produto
          else
            det = det + v(i, col_paridade) * produto
          end if
      end do

      write(*,*) "Determinante calculado:", det

      stop
      end
