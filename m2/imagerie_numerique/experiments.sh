#!/bin/sh

###
# This script demands from the user the
# experiment to perform, and executes the
# corresponding code on a list of images
# inside the corresponding folders. See
# the README for details on folder structure.
###

echo -n "Enter a mode of experiment (synthesis, transfer, eiffel)"
read MODE_EXP

case $MODE_EXP in
	
	###
	# TEXTURE SYNTHESIS
	###
synthesis)
	echo -n "Performing texture synthesis."
	for fol in Styles/*/
	do
		for file_ in "$fol"* 
		do
			python optex.py -s "$file_" --output_dir "Outputs/"
		done
	done
	;;
	
	###
	# STYLE TRANSFER
	###
transfer)
	echo -n "Performing style transfer."
	for content in Contents/*
	do
		for style in Styles/Transfer/*
		do
			python optex.py -s $style -c $content --content_strength 0.07 --output_dir Outputs/Transfer/
		done
	done
	;;
	
	###
	# EIFFEL TOWER EXPERIMENT (CONTENT STRENGTH)
	###
eiffel)
	for content in Contents/*
	do
		for style in Styles/Transfer/*
		do
			for intensity in 0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09  
			do
				python optex.py -s $style -c $content --content_strength $intensity --output_dir Outputs/Transfer/
			done
		done
	done
	;;
esac