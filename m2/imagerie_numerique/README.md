# Imagerie Numérique

Students: Bianca MARIN MORENO, Leonardo MARTINS BIANCO.

## Introduction

This repo contains our code for the final project of the course "Imagerie Numérique" at the M2 MVA. Beyond our created and modified code, we provide the folder structure with which the user can perform his own experiments of texture synthesis, style transfer, and variation of the parameter of content strength.

We provide the styles we used for texture synthesis and style transfer, and the contents for style transfer. 

We **do not** provide here the outputs, as they are present in the report attached and take up considerable memory. Most importantly, we **do not** provide the pre-trained encoders and decoders, as they take too much memory. Consider cloning the [github][https://github.com/JCBrouwer/OptimalTextures] from which we started and copying the ``models`` folder.

## Experiments

### Folder structure

We indicate below which files contain original code.

```python
# root/
# |- Styles/
# |- Contents/
# |- Outputs/
# |- models/
# |- histmatch.py
# |- optex.py
# |- optex_sliced_ot.py ## (ORIGINAL)
# |- util.py
# |- utils_sliced_ot.py ## (ORIGINAL)
# |- gram_matrices.py ## (ORIGINAL)
# |- vgg.py
# |- experiments.sh
# |- requirements.txt
# |- report.pdf
# |- README.md
```

## Running the shell script

To run experiments as we did, simply run the executable ``experiments.sh`` and answer the prompt that appears.

* Enter ``synthesis`` to perform texture synthesis with all the textures present in the subfolders of ``Styles/``. The outputs are saved to ``Outputs/Synthesis/``.
* Enter ``transfer`` and then the content strength to perform style transfer with all the base content present in the folder ``Contents/`` and the styles present in ``Styles/Transfer/``. The outputs are saved to ``Outputs/Transfer/``.
* Enter ``eiffel`` to perform the experiment of varying content strength with the base content being ``Contents/content_6.jpg`` (eiffel tower) and the style being ``Styles/Transfer/style_3.png``. The outputs are saved to ``Outputs/Eiffel/``.
