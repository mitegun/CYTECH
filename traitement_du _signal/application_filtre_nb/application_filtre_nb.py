import matplotlib.pyplot as plt
from PIL import Image

def produit_convo(pixel, noyau, largeur, hauteur):
    newpixel = []
    for p in range(largeur * hauteur):
        s = 0
        for i in range(5):
            for j in range(5):
                x = (p % largeur) + (j - 2)
                y = (p // largeur) + (i - 2)

                if 0 <= x < largeur and 0 <= y < hauteur:
                    s += noyau[i * 5 + j] * pixel[y * largeur + x]

        newpixel.append(s)
    return newpixel
def image_floue():
    noyau=[1/25, 1/25, 1/25, 1/25, 1/25,
             1/25, 1/25, 1/25, 1/25, 1/25,
            1/25, 1/25, 1/25, 1/25, 1/25,
             1/25, 1/25, 1/25, 1/25, 1/25,
             1/25, 1/25, 1/25, 1/25, 1/25]
    ListePixelsFiltres = produit_convo(ListePixels, noyau, w, h)
    img_reparee = Image.new("L", (w, h))
    img_reparee.putdata(ListePixelsFiltres)
    img_reparee.save("..\Traitement_Signal_DAHMANI_KEMGNE_VISOMBLAIN_COUDERC_SOUARE_GOLOTVINE\\traitement_du _signal\\application_filtre_nb\image_filtre_flou.jpg")
    return img_reparee


def image_contraste():
    noyau = [0, 0, 0, 0, 0,
             0, 0,-1, 0, 0,
             0, -1, 5,-1,0,
             0, 0, -1, 0, 0,
             0, 0, 0, 0, 0]
    ListePixelsFiltres = produit_convo(ListePixels, noyau, w, h)
    img_reparee = Image.new("L", (w, h))
    img_reparee.putdata(ListePixelsFiltres)
    img_reparee.save("..\Traitement_Signal_DAHMANI_KEMGNE_VISOMBLAIN_COUDERC_SOUARE_GOLOTVINE\\traitement_du _signal\\application_filtre_nb\image_filtre_contraste.jpg")

    return img_reparee
def image_Sobel():
    noyau = [0, 0, 0, 0, 0,
            0, -1, 0, 1, 0,
            0, -2, 0, 2,0,
            0, -1, 0, 0, 0,
            0, 0, 0, 0, 0]
    ListePixelsFiltres = produit_convo(ListePixels, noyau, w, h)
    img_reparee = Image.new("L", (w, h))
    img_reparee.putdata(ListePixelsFiltres)
    img_reparee.save("..\Traitement_Signal_DAHMANI_KEMGNE_VISOMBLAIN_COUDERC_SOUARE_GOLOTVINE\\traitement_du _signal\\application_filtre_nb\image_filtre_sobel.jpg")

    return img_reparee

def renforcement_bord():
    noyau = [0, 0, 0, 0, 0,
             0, 0, 0, 0, 0,
             0, -1, 1, 0, 0,
             0, 0, 0, 0, 0,
             0, 0, 0, 0, 0]
    ListePixelsFiltres = produit_convo(ListePixels, noyau, w, h)
    img_reparee = Image.new("L", (w, h))
    img_reparee.putdata(ListePixelsFiltres)
    img_reparee.save("..\Traitement_Signal_DAHMANI_KEMGNE_VISOMBLAIN_COUDERC_SOUARE_GOLOTVINE\\traitement_du _signal\\application_filtre_nb\image_filtre_renforcement_bord.jpg")

    return img_reparee

def detect_bord():
    noyau = [0, 0, 0, 0, 0,
             0, 0, 1, 0, 0,
             0, 1, -4, 1, 0,
             0, 0, 1, 0, 0,
             0, 0, 0, 0, 0]
    ListePixelsFiltres = produit_convo(ListePixels, noyau, w, h)
    img_reparee = Image.new("L", (w, h))
    img_reparee.putdata(ListePixelsFiltres)
    img_reparee.save("..\Traitement_Signal_DAHMANI_KEMGNE_VISOMBLAIN_COUDERC_SOUARE_GOLOTVINE\\traitement_du _signal\\application_filtre_nb\image_filtre_detect_bord.jpg")

    return img_reparee


# Chargement de l'image en noir et blanc
im = Image.open("..\Traitement_Signal_DAHMANI_KEMGNE_VISOMBLAIN_COUDERC_SOUARE_GOLOTVINE\\traitement_du _signal\\application_filtre_nb\image_nb.jpg").convert("L")
w, h = im.size

# Transformation de l'image en liste de pixels
ListePixels = list(im.getdata())
# Affichage des images avant et après le filtre
fig, axs = plt.subplots(3, 3, figsize=(10, 5))
plt.axis('off')
axs[0][0].imshow(im, cmap="gray")
axs[0][0].set_title('Image originale')
axs[0][1].imshow(image_floue(), cmap="gray")
axs[0][1].set_title('Filtre flou')
axs[0][2].imshow(image_contraste(), cmap="gray")
axs[0][2].set_title('Filtre augmentant le contraste')
axs[1][0].imshow(image_Sobel(), cmap="gray")
axs[1][0].set_title('Filtre de Sobel')
axs[1][1].imshow(renforcement_bord(), cmap="gray")
axs[1][1].set_title('Filtre Renforçant les bords')
axs[1][2].imshow(detect_bord(), cmap="gray")
axs[1][2].set_title('Filtre Détectant les bords')
fig.delaxes(axs[2][0])
fig.delaxes(axs[2][1])
fig.delaxes(axs[2][2])
for j in range(0,2):
    axs[j][0].axis("off")
    axs[j][1].axis("off")
    axs[j][2].axis("off")

plt.show()

