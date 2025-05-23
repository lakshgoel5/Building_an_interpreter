#!/bin/bash

# Assuming your OCaml executable is called "parser" and is in the current directory
EXECUTABLE="./main.exe"

# Create directory for output files if it doesn't exist
mkdir -p testcases

# Process each input file from 1 to 10
# for i in {1..10}; do
#     INPUT_FILE="testcases/input$i.txt"
#     OUTPUT_FILE="testcases/expected_input$i.txt"
    
#     echo "Processing $INPUT_FILE -> $OUTPUT_FILE"
    
#     # Modify this command to point to your actual OCaml executable
#     # and adjust any arguments it needs
#     $EXECUTABLE "$INPUT_FILE" > "$OUTPUT_FILE" 2>&1
    
#     if [ $? -eq 0 ]; then
#         echo "Successfully processed $INPUT_FILE"
#     else
#         echo "Error processing $INPUT_FILE"
#     fi
# done

# # Process each input file from 1 to 10
# for i in {1..11}; do
#     INPUT_FILE="ta_testcases/ta$i.txt"
#     OUTPUT_FILE="ta_testcases/output_ta$i.txt"
    
#     echo "Processing $INPUT_FILE -> $OUTPUT_FILE"
    
#     # Modify this command to point to your actual OCaml executable
#     # and adjust any arguments it needs
#     $EXECUTABLE "$INPUT_FILE" > "$OUTPUT_FILE" 2>&1
    
#     if [ $? -eq 0 ]; then
#         echo "Successfully processed $INPUT_FILE"
#     else
#         echo "Error processing $INPUT_FILE"
#     fi
# done

# for i in {1..8}; do
#     INPUT_FILE="moreTests/input$i.txt"
#     OUTPUT_FILE="moreTests/output$i.txt"
    
#     echo "Processing $INPUT_FILE -> $OUTPUT_FILE"
    
#     # Modify this command to point to your actual OCaml executable
#     # and adjust any arguments it needs
#     $EXECUTABLE "$INPUT_FILE" > "$OUTPUT_FILE" 2>&1
    
#     if [ $? -eq 0 ]; then
#         echo "Successfully processed $INPUT_FILE"
#     else
#         echo "Error processing $INPUT_FILE"
#     fi
# done

for i in {1..9}; do
    INPUT_FILE="A3_testcases/t$i.dsl"
    OUTPUT_FILE="A3_testcases/output$i.dsl"
    
    echo "Processing $INPUT_FILE -> $OUTPUT_FILE"

    $EXECUTABLE "$INPUT_FILE" > "$OUTPUT_FILE" 2>&1
    
    if [ $? -eq 0 ]; then
        echo "Successfully processed $INPUT_FILE"
    else
        echo "Error processing $INPUT_FILE"
    fi
done