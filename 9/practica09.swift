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
