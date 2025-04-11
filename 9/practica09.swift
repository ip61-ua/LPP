/**
 * Ejercicio 1
 */

// print("Hola, mundo")

// Exp. 1 y 2
/*
let miFloat: Float = 4
var miEtiqueta = "El valor es "

var miEtiqueta2 = miEtiqueta

// cannot convert value of type 'Float' to expected argument Al hacerlo sin String
miEtiqueta = miEtiqueta + String(miFloat) + " " + miEtiqueta  // + = append
//miEtiqueta.append(miFloat)

//++miEtiqueta // da error
//miEtiqueta = miEtiqueta - miEtiqueta // da error
print("1 |", miEtiqueta)

miEtiqueta2 = "\(miEtiqueta2)\(miFloat)"
print("2 |", miEtiqueta2)
*/

// Exp. 3
/*
let nameSalute = "internauta"
let miResult: Float = 9 / 2

print("Resultado: \(miResult * 2) = 9")
print("¡Saludos, \(nameSalute)!")
*/

// Exp. 4
/*
var cadenaOpcional: String? = "Hola"
print(cadenaOpcional == nil)

var nombreOpcional: String? = nil
var saludo = "Hola!"
if let nombre = nombreOpcional {
  saludo = "Hola, \(nombre)"
} else {
  saludo += saludo  // si es nil, sale esto
}
print(saludo)
*/

// Exp. 5
/*
let nombrePila: String? = nil
let nombreCompleto: String = "John Appleseed"
let saludoInformal = "¿Qué tal, \(nombrePila == nil ? "undefined" : nombreCompleto)?"
print(saludoInformal)

let verdura = "zanahoria"
switch verdura {  // Switch must be exhaustive
case "zanahoria":
  print("Buena para la vista.")
case "lechuga", "tomates":
  print("Podrías hacer una buena ensalada.")
default:
  print("Siempre puedes hacer una buena sopa.")
}
print("Hola desde casa")
*/

// Exp. 6
/*
let numerosInteresantes = [
  "Primos": [2, 3, 5, 7, 11, 13],
  "Fibonacci": [1, 1, 2, 3, 5, 8],
  "Cuadrados": [1, 4, 9, 16, 25],
]
var mayor = 0
var mayorString = ""

for (clase, numeros) in numerosInteresantes {
  for num in numeros {
    if num > mayor {
      mayor = num
      mayorString = clase
    }
  }
}
print("\"\(mayorString)\": \(mayor)")
*/

// Exp. 7
/*
var n = 2
while n < 100 {
  n *= 2
}
print(n)

var m = 2
repeat {
  m *= 2
} while m < 100
print(m)

var total = 0
for _ in -1..<4 {
  total += 1
}
print(total)

func saluda(nombre: String, comida: String = "burger cangreburger") -> String {
  return "Hola \(nombre), tienes para hoy \(comida)."
}
print(saluda(nombre: "Bob"))
*/

// Exp. 8
/*
func saluda(_ nombre: String, el dia: String) -> String {
  return "Hola \(nombre), hoy es \(dia)."
}
print(saluda("Bob", el: "Martes"))

func
  calculaEstadisticas(puntuaciones: [Int])
  -> (min: Int, max: Int, sum: Int)
{
  var min = puntuaciones[0]
  var max = puntuaciones[0]
  var sum = 0

  for puntuacion in puntuaciones {
    if puntuacion > max {
      max = puntuacion
    } else if puntuacion < min {
      min = puntuacion
    }
    sum += puntuacion
  }

  return (min, max, sum)
}

let estadisticas = calculaEstadisticas(puntuaciones: [5, 3, 100, 3, 9])
print(estadisticas)

func suma(_ numeros: Int...) -> Float {
  var suma = 0
  if numeros.count == 0 {
    return 0
  }

  for num in numeros {
    suma += num
  }
  let res = Float(suma) / Float(numeros.count)
  return res
}
print(suma())
print(suma(42, 597, 12))
*/

// Exp. 9
/*
func devuelveQuince() -> Int {
  var y = 10
  func suma() {
    y += 5
  }
  suma()
  return y
}
print(devuelveQuince())

func construyeIncrementador() -> ((Int) -> Int) {
  func sumaUno(numero: Int) -> Int {
    return 1 + numero
  }
  return sumaUno
}
var incrementa = construyeIncrementador()
print(incrementa(incrementa(7)))

func devuelveSuma() -> (() -> Int) {
  var y = 10
  func suma() -> Int {
    y += 5
    return y
  }
  return suma
}

let f = devuelveSuma()
print(f())
print(f())
print(f())

func cumpleCondicion(lista: [Int], condicion: (Int) -> Bool) -> Bool {
  for item in lista {
    if condicion(item) {
      return true
    }
  }
  return false
}
func menorQueDiez(numero: Int) -> Bool {
  return numero < 10
}
var numeros = [20, 19, 7, 12]
print(cumpleCondicion(lista: numeros, condicion: menorQueDiez))

let d = numeros.map({ numero in
  numero % 2 == 0 ? 3 * numero : 0
})
print(d)

let x = numeros.map { $0 % 2 == 0 ? $0 : 0 }
print(d)
*/

/**
 * Ejercicio 2
 */

// a
func
  prefijos(prefijo: String, palabras: [String])
  -> [Bool]
{
  if palabras.isEmpty {
    return []
  }

  var res = [palabras[0].hasPrefix(prefijo)]
  res.append(
    contentsOf:
      prefijos(
        prefijo: prefijo, palabras: Array(palabras.dropFirst())
      ))
  return res
}

let array = ["anterior", "antígona", "antena"]
let prefijo = "ante"
/*
print("\n******\n2a) Función prefijos(prefijo:palabras:)\n******")
print(prefijos(prefijo: prefijo, palabras: array))
// Imprime: [true, false, true]
*/

// b
func
  parejaMayorParImpar(numeros: [Int])
  -> (Int, Int)
{
  if numeros.isEmpty {
    return (0, 0)
  }

  let myNum = numeros[0]
  let rest = parejaMayorParImpar(numeros: Array(numeros.dropFirst()))

  if myNum % 2 == 0 && myNum > rest.1 {
    return (rest.0, myNum)
  } else if myNum % 2 != 0 && myNum > rest.0 {
    return (myNum, rest.1)
  }

  return (rest.0, rest.1)
}

let numeros = [10, 201, 12, 103, 204, 2]
/*
print("\n******\n2b) Función parejaMayorParImpar(numeros:)\n******")
print(parejaMayorParImpar(numeros: numeros))
*/
// Imprime: (201, 204)

/**
 * Ejercicio 3
 */

// a
func
  compruebaParejas(_ lista: [Int], funcion: ((Int) -> Int))
  -> [(Int, Int)]
{
  var pairResult: [(Int, Int)] = []

  if lista.isEmpty {
    return pairResult
  }

  let a = lista[0]
  let b = funcion(a)

  for elem in lista {
    if b == elem {
      pairResult = [(a, b)]
      break
    }
  }

  pairResult.append(contentsOf: compruebaParejas(Array(lista.dropFirst()), funcion: funcion))
  return pairResult
}

func cuadrado(x: Int) -> Int {
  return x * x
}
/*
print(compruebaParejas([2, 4, 16, 5, 10, 100, 105], funcion: cuadrado))
// Imprime [(2,4), (4,16), (10,100)]
*/

// b
func
  coinciden(parejas: [(Int, Int)], funcion: (Int) -> Int)
  -> [Bool]
{
  if parejas.isEmpty {
    return []
  }

  let miTupla = parejas[0]
  let coincideTupla = funcion(miTupla.0) == miTupla.1

  return [coincideTupla] + coinciden(parejas: Array(parejas.dropFirst()), funcion: funcion)
}

/*
let array1 = [(2, 4), (4, 14), (4, 16), (5, 25), (10, 100)]
print(coinciden(parejas: array1, funcion: cuadrado))
// Imprime: [true, false, true, true, true]
*/

enum Movimiento {
  case deposito(Double)
  case cargoRecibo(String, Double)
  case cajero(Double)
}

func
  aplica(movimientos: [Movimiento])
  -> (Double, [String])
{
  var result: (Double, [String]) = (0.0, [])

  for move in movimientos {
    switch move {
    case let .deposito(money):
      result.0 += money
    case let .cargoRecibo(trans, money):
      result.0 -= money
      result.1.append(trans)
    case let .cajero(money):
      result.0 -= money
    }
  }

  return result
}

let movimientos: [Movimiento] = [
  .deposito(830.0), .cargoRecibo("Gimnasio", 45.0), .deposito(400.0), .cajero(100.0),
  .cargoRecibo("Fnac", 38.70),
]
print(aplica(movimientos: movimientos))
//Imprime (1046.3, ["Gimnasio", "Fnac"])
