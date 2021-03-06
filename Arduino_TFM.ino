/********************************************************************/
// Incluimos las librerías del sistema OneWire para leer los 12 bits de
// datos de la sonda térmica y la librería para traducirlo a grados
#include <OneWire.h> 
#include <DallasTemperature.h>
/********************************************************************/
// Se define el puerto de entrada de datos digitales (temp)
#define ONE_WIRE_BUS 2 
int analogPin = A0;
int sensorValue = 0;
float voltage = 0.0;
float temp_obs = 0.0;
// Inicio del bus de datos de sistema OneWire para comunicarse
OneWire oneWire(ONE_WIRE_BUS); 
// Conversión de los bytes a magnitud numérica en grados
DallasTemperature sensors(&oneWire);
/********************************************************************/ 
// Código de inicio
void setup(void) 
{ 
 // Abrimos el puerto serie para lectura
 Serial.begin(9600); 
 Serial.println("Dallas Temperature IC Control Library for DS18B20"); 
 // Abrimos la librería de lectura de sensores
 sensors.begin(); 
} 
/********************************************************************/
// Código continuo en bucle
void loop(void) 
{ 
 /********************************************************************/
  // Parte de la temperatura
  // Hace falta enviar el comando sensors.requestTemperatures() para
  // recibir la lectura
  Serial.print(" Requesting temperatures..."); 
  sensors.requestTemperatures(); // Send the command to get temperature readings 
  Serial.println("DONE"); 
  // Lectura de temperatura
  Serial.print("Temperature is: "); 
  Serial.println(sensors.getTempCByIndex(0)); 
  // Index 0 porque se puede tener varios onewire por bus, el primero
  // y único es el de temperatura que es el i=0
  /*******************************************************************/
  // Parte del sensor lineal
  // Leemos el voltaje (analógico) por el pin A0:
  int sensorValue = analogRead(analogPin);
  // Convertimos la lectura en analógico que corresponde a un valor
  // Entre 0 y 1023 (2^10, 10 bits) es la escala
  // Si alimentamos el sensor con la salida de 3.3 V entonces hay
  // una escala lineal donde 0 -> 0 V y 1023 -> 3.3 V 
  // Se hace la transformación lineal donde para un voltaje V tenemos
  // pdte = (3.3-0)/(1023-0) , ord origen = 0
  // V=3.3/1023*A0+0
  float voltage = sensorValue * (5.000 / 1023);
  // Print del valor
  Serial.print("Sensor reading is: ");
  Serial.print(sensorValue);
  Serial.print("   ");
  Serial.print(voltage);
  float sensor = sensorValue;
  // Si la lectura no está en los extremos entonces vale
  if (sensor < 940 and sensor > 100) {
    // Aproximación polinómica Analogico0-Temperatura
    float A = 7.161e+01;
    float B = -1.636e-01;
    float C = 2.337e-04;
    float D = -1.365e-07;
    float temp_obs = A+B*sensor+C*pow(sensor,2)+D*pow(sensor,3);
    Serial.print("    ");
    Serial.print(temp_obs);
    Serial.println(" ºC read");
  }
  // Las medidas en los extremos se desechan como ruido
  if (sensor >= 920 or sensor < 120) {
    Serial.println("    none ºC read");
  }
  //Repetir cada segundo
  delay(1000);
}
