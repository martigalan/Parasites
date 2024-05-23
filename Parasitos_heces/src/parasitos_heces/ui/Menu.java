package parasitos_heces.ui;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Scanner;
import java.util.UUID;

public class Menu {

	public static void main(String[] args) {
		Scanner scanner = new Scanner(System.in);
		HashMap<String,String> diccionario = new HashMap<>();
		File currentWorkingDirectory = new File("resources"); //creo un archivo resources
		File[] filesInResources = currentWorkingDirectory.listFiles((dir, name) -> name.toLowerCase().endsWith(".jpg"));
		/*for (File file : filesInResources) {
			System.out.println(file.getName()); 
		}*/
		
		System.out.println("Ingrese su nombre:");
        String userName = scanner.nextLine();
        Date fechaActual = new Date();
        // Formatear la fecha actual en el formato deseado
        SimpleDateFormat formatoFecha = new SimpleDateFormat("dd-MM-yy");
        String fechaFormateada = formatoFecha.format(fechaActual);
        
        // Crear la carpeta en el directorio de trabajo 
        File experimento_user = new File("experimento_" + userName + "_" + fechaFormateada);
        experimento_user.mkdir(); 
              
        // Copiar archivos desde "resources" a la nueva carpeta "experimento_user"
        if (filesInResources != null) {
        	String[] extensiones = {".jpg",".xml"};
            for (File file : filesInResources) {
            	if (file.isFile()) {
                        Path origen = file.toPath();
                        String nombreArchivo = obtenerNombreArchivo(file);
                        guardarEnDiccionario(diccionario,nombreArchivo);
                        generarArchivos(origen, extensiones, experimento_user, diccionario, nombreArchivo);
                }
            }
        migrarXMLS(experimento_user);  
        }     
	}
	
	public static void generarArchivos(Path origen, String[] extensiones, File experimento_user, HashMap<String,String> diccionario, String nombreArchivo) {
        for (String extension : extensiones) {
            Path destino = new File(experimento_user,diccionario.get(nombreArchivo)+extension).toPath();
            escribirCorrespondencia(diccionario,experimento_user); 
            try {
                Files.copy(origen, destino, StandardCopyOption.REPLACE_EXISTING); //así evitamos sobreescribir datos ya existentes
            } catch (IOException ex) {
                System.out.println(ex);;
            }
        
        }
	}
	
	public static void migrarXMLS(File experimento_user) {
		File anotaciones_expertos = null;
        if (experimento_user.exists()) {
            anotaciones_expertos = new File(experimento_user, "anotaciones_expertos");
            anotaciones_expertos.mkdir();
        }
        File[] xmlFiles = experimento_user.listFiles((dir, name) -> name.toLowerCase().endsWith(".xml"));
        /*for (File file : xmlFiles) {
			System.out.println(file.getName());
		}*/
        
        // Mover cada archivo XML a la carpeta "anotaciones_expertos"
	        if (xmlFiles != null) {
	            for (File xmlFile : xmlFiles) {
	                try {
	                    Path origen = xmlFile.toPath();
	                    Path destino = new File(anotaciones_expertos, xmlFile.getName()).toPath();
	                    Files.move(origen, destino, StandardCopyOption.REPLACE_EXISTING);
	                    //System.out.println("Archivo " + xmlFile.getName() + " movido correctamente.");
	                } catch (IOException e) {
	                	e.printStackTrace();
	                }
	            }
	        }
		}
	
        public static String obtenerNombreArchivo(File file) {
            String[] separacionArchivo = file.getName().split("\\."); 
            return separacionArchivo[separacionArchivo.length-2];
        }
        
        public static void guardarEnDiccionario(HashMap<String,String> diccionario, String nombreArchivo) {
            // Renombro archivos de mi carpeta con números aleatorios
            if(!diccionario.containsKey(nombreArchivo)) {
            	String nuevoNombre = "realId-"+generateRandomUUID(); //Generar un nuevo nombre aleatorio 
            	diccionario.put(nombreArchivo,nuevoNombre); //Añadirlo al diccionario comparado con el original
            }
        }
        	
	    private static String generateRandomUUID(){
	    	return UUID.randomUUID().toString();
	    }  
		
		private static void escribirCorrespondencia(HashMap<String, String> correspondencia, File experimento_user) {
        File archivoCorrespondencia = new File(experimento_user + "/correspondencia.txt"); 
        try (BufferedWriter escritor = new BufferedWriter(new FileWriter(archivoCorrespondencia))) {
            for (HashMap.Entry<String, String> entry : correspondencia.entrySet()) {
                escritor.write(entry.getKey() + ";" + entry.getValue() + "\n");
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
