import os
import pydicom
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.widgets import Slider

# Función para cargar todas las imágenes DICOM de un directorio
def load_dicom_images(directory):
    dicom_files = [os.path.join(directory, f) for f in os.listdir(directory) if f.endswith('.dcm')]
    dicom_files.sort()  # Asegurarse de que estén en orden
    dicom_images = [pydicom.dcmread(f) for f in dicom_files]
    return dicom_images

# Directorio donde están tus archivos DICOM
dicom_directory = '/home/camilo/LaverGit/Data_analytic_and_science_project/ProjectData_Analytic_and_Science/Project_Python/ProcesarImagenResonanciaMagnetica /AVILAMORAROBERT'
   # Cambia esto a la ruta correcta

# Cargar las imágenes DICOM
dicom_images = load_dicom_images(dicom_directory)

# Crear la figura y el eje
fig, ax = plt.subplots()
plt.subplots_adjust(bottom=0.25)  # Ajustar espacio para el slider

# Mostrar la primera imagen
current_index = 0
im = ax.imshow(dicom_images[current_index].pixel_array, cmap='gray')
ax.set_title(f'Capa {current_index + 1}')
ax.axis('off')

# Crear el slider
ax_slider = plt.axes([0.25, 0.1, 0.65, 0.03])  # Posición y tamaño del slider
slider = Slider(ax_slider, 'Capa', 0, len(dicom_images) - 1, valinit=0, valstep=1)

# Función para actualizar la imagen cuando se mueve el slider
def update(val):
    index = int(slider.val)
    im.set_data(dicom_images[index].pixel_array)
    ax.set_title(f'Capa {index + 1}')
    fig.canvas.draw()

# Conectar el slider a la función de actualización
slider.on_changed(update)

# Mostrar la ventana
plt.show()