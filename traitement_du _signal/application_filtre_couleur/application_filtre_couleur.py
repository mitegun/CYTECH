from PIL import Image                                    
import matplotlib.pyplot as plt



def produit_convo(pixel,noyau,largeur,hauteur):
    newpixel=[]
    for p in range(largeur*hauteur):
        s=0
        for i in range(5):
            if p-2*largeur>=0:
                if abs((p-2*largeur-2+i)%largeur-(p-2*largeur)%largeur)<=2 :
                    s=s+noyau[i]*pixel[p-2*largeur-2+i]
                else:
                    continue
            else:
                break
        for i in range(5):
            if p-largeur>=0:
                if abs((p-largeur-2+i)%largeur-(p-largeur)%largeur)<=2 :
                    s=s+noyau[5+i]*pixel[p-largeur-2+i]
                else:
                    continue
            else:
                break
        for i in range(5):
            if abs((p-2+i)%largeur-p%largeur)<=2 :
                    s=s+noyau[10+i]*pixel[p-2+i]
            else:
                continue
        for i in range(5):
            if p+largeur<largeur*hauteur:
                if abs((p+largeur-2+i)%largeur-(p+largeur)%largeur)<=2 :
                    s=s+noyau[15+i]*pixel[p+largeur-2+i]
                else:
                    continue
            else:
                break
        for i in range(5):
            if p+2*largeur<largeur*hauteur:
                if abs((p+2*largeur-2+i)%largeur-(p+2*largeur)%largeur)<=2 :
                    s=s+noyau[20+i]*pixel[p+2*largeur-2+i]
                else:
                    continue
            else:
                break
        newpixel.append(s)
    return newpixel

 
# Définition du noyau
def image_floue():
    noyau=[1/25, 1/25, 1/25, 1/25, 1/25,
             1/25, 1/25, 1/25, 1/25, 1/25,
            1/25, 1/25, 1/25, 1/25, 1/25,
             1/25, 1/25, 1/25, 1/25, 1/25,
             1/25, 1/25, 1/25, 1/25, 1/25]
    ListePixelsRouges=produit_convo(ListePixelsRouges_img,noyau,w,h)
    ListePixelsVerts=produit_convo(ListePixelsVerts_img,noyau,w,h)
    ListePixelsBleus=produit_convo(ListePixelsBleus_img,noyau,w,h)
    ImagePixelsRouges.putdata(ListePixelsRouges)
    ImagePixelsVerts.putdata(ListePixelsVerts)
    ImagePixelsBleus.putdata(ListePixelsBleus)
    imgnew = Image.merge('RGB',(ImagePixelsRouges,ImagePixelsVerts,ImagePixelsBleus))
    imgnew.save("..\Traitement_Signal_DAHMANI_KEMGNE_VISOMBLAIN_COUDERC_SOUARE_GOLOTVINE\\traitement_du _signal\\application_filtre_couleur\image_filtre_flou.jpg")
    return imgnew

def image_contraste():
    noyau = [0, 0, 0, 0, 0,
             0, 0,-1, 0, 0,
             0, -1, 5,-1,0,
             0, 0, -1, 0, 0,
             0, 0, 0, 0, 0]
    ListePixelsRouges=produit_convo(ListePixelsRouges_img,noyau,w,h)
    ListePixelsVerts=produit_convo(ListePixelsVerts_img,noyau,w,h)
    ListePixelsBleus=produit_convo(ListePixelsBleus_img,noyau,w,h)
    ImagePixelsRouges.putdata(ListePixelsRouges)
    ImagePixelsVerts.putdata(ListePixelsVerts)
    ImagePixelsBleus.putdata(ListePixelsBleus)
    imgnew = Image.merge('RGB',(ImagePixelsRouges,ImagePixelsVerts,ImagePixelsBleus))
    imgnew.save("..\Traitement_Signal_DAHMANI_KEMGNE_VISOMBLAIN_COUDERC_SOUARE_GOLOTVINE\\traitement_du _signal\\application_filtre_couleur\image_filtre_contraste.jpg")
    return imgnew 

def image_Sobel():
    noyau = [0, 0, 0, 0, 0,
            0, -1, 0, 1, 0,
            0, -2, 0, 2,0,
            0, -1, 0, 0, 0,
            0, 0, 0, 0, 0]
    ListePixelsRouges=produit_convo(ListePixelsRouges_img,noyau,w,h)
    ListePixelsVerts=produit_convo(ListePixelsVerts_img,noyau,w,h)
    ListePixelsBleus=produit_convo(ListePixelsBleus_img,noyau,w,h)
    ImagePixelsRouges.putdata(ListePixelsRouges)
    ImagePixelsVerts.putdata(ListePixelsVerts)
    ImagePixelsBleus.putdata(ListePixelsBleus)
    imgnew = Image.merge('RGB',(ImagePixelsRouges,ImagePixelsVerts,ImagePixelsBleus))
    imgnew.save("..\Traitement_Signal_DAHMANI_KEMGNE_VISOMBLAIN_COUDERC_SOUARE_GOLOTVINE\\traitement_du _signal\\application_filtre_couleur\image_filtre_sobel.jpg")
    return imgnew

def renforcement_bord():
    noyau = [0, 0, 0, 0, 0,
             0, 0, 0, 0, 0,
             0, -1, 1, 0, 0,
             0, 0, 0, 0, 0,
             0, 0, 0, 0, 0]
    ListePixelsRouges=produit_convo(ListePixelsRouges_img,noyau,w,h)
    ListePixelsVerts=produit_convo(ListePixelsVerts_img,noyau,w,h)
    ListePixelsBleus=produit_convo(ListePixelsBleus_img,noyau,w,h)
    ImagePixelsRouges.putdata(ListePixelsRouges)
    ImagePixelsVerts.putdata(ListePixelsVerts)
    ImagePixelsBleus.putdata(ListePixelsBleus)
    imgnew = Image.merge('RGB',(ImagePixelsRouges,ImagePixelsVerts,ImagePixelsBleus))
    imgnew.save("..\Traitement_Signal_DAHMANI_KEMGNE_VISOMBLAIN_COUDERC_SOUARE_GOLOTVINE\\traitement_du _signal\\application_filtre_couleur\image_filtre_renforcement_bord.jpg")
    return imgnew

def detect_bord():
    noyau = [0, 0, 0, 0, 0,
             0, 0, 1, 0, 0,
             0, 1, -4, 1, 0,
             0, 0, 1, 0, 0,
             0, 0, 0, 0, 0]
    ListePixelsRouges=produit_convo(ListePixelsRouges_img,noyau,w,h)
    ListePixelsVerts=produit_convo(ListePixelsVerts_img,noyau,w,h)
    ListePixelsBleus=produit_convo(ListePixelsBleus_img,noyau,w,h)
    ImagePixelsRouges.putdata(ListePixelsRouges)
    ImagePixelsVerts.putdata(ListePixelsVerts)
    ImagePixelsBleus.putdata(ListePixelsBleus)
    imgnew = Image.merge('RGB',(ImagePixelsRouges,ImagePixelsVerts,ImagePixelsBleus))
    imgnew.save("..\Traitement_Signal_DAHMANI_KEMGNE_VISOMBLAIN_COUDERC_SOUARE_GOLOTVINE\\traitement_du _signal\\application_filtre_couleur\image_filtre_detect_bord.jpg")
    return imgnew
def repoussage():
    noyau = [0, 0, 0, 0, 0,
             0, -2, -1, 0, 0,
             0, -1, 1, 1, 0,
             0, 0, 1, 2, 0,
             0, 0, 0, 0, 0]
    ListePixelsRouges=produit_convo(ListePixelsRouges_img,noyau,w,h)
    ListePixelsVerts=produit_convo(ListePixelsVerts_img,noyau,w,h)
    ListePixelsBleus=produit_convo(ListePixelsBleus_img,noyau,w,h)
    ImagePixelsRouges.putdata(ListePixelsRouges)
    ImagePixelsVerts.putdata(ListePixelsVerts)
    ImagePixelsBleus.putdata(ListePixelsBleus)
    imgnew = Image.merge('RGB',(ImagePixelsRouges,ImagePixelsVerts,ImagePixelsBleus))
    imgnew.save("..\Traitement_Signal_DAHMANI_KEMGNE_VISOMBLAIN_COUDERC_SOUARE_GOLOTVINE\\traitement_du _signal\\application_filtre_couleur\image_filtre_repoussage.jpg")
    return imgnew

# Chargement de l'image en couleur
im = Image.open("..\Traitement_Signal_DAHMANI_KEMGNE_VISOMBLAIN_COUDERC_SOUARE_GOLOTVINE\\traitement_du _signal\\application_filtre_couleur\image_couleur.jpg")                                         
w,h=im.size
# Transformation de l'image en liste de pixels Rouge, Vert et Bleu
ImagePixelsRouges,ImagePixelsVerts,ImagePixelsBleus=im.split()      
ListePixelsRouges_img=list(ImagePixelsRouges.getdata())                 
ListePixelsVerts_img=list(ImagePixelsVerts.getdata())
ListePixelsBleus_img=list(ImagePixelsBleus.getdata())
fig, axs = plt.subplots(3, 3, figsize=(10, 5))
# Affichage des images avant et après le filtre
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
axs[2][0].imshow(repoussage(), cmap="gray")
axs[2][0].set_title('Filtre Repoussage')
fig.delaxes(axs[2][1])
fig.delaxes(axs[2][2])
for j in range(0,3):
    axs[j][0].axis("off")
    axs[j][1].axis("off")
    axs[j][2].axis("off")
plt.show()
im.close()
