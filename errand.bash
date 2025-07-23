#!/bin/bash

# Function to show usage
show_usage() {
    echo "Usage: $0 <command> <file> [--arm]"
    echo "Commands:"
    echo "  compile <file> [--arm]  - Compile the .err file to an executable"
    echo "  run <file> [--arm]      - Compile and run the .err file"
    echo ""
    echo "Examples:"
    echo "  $0 compile test_simple.err"
    echo "  $0 run test_simple.err --arm"
    exit 1
}

# Check if correct number of arguments is provided
if [ $# -lt 2 ]; then
    show_usage
fi

# Get the command and file
command="$1"
input_file="$2"

# Check for optional --arm or --x86 flag
arch_flag=""
if [ "$3" == "--arm" ] || [ "$3" == "--x86" ]; then
    arch_flag="$3"
fi

# Check if the file exists
if [ ! -f "$input_file" ]; then
    echo "Error: File '$input_file' not found"
    exit 1
fi

# Extract the base name without extension for the output binary
base_name=$(basename "$input_file" .err)

# Function to compile the file
compile_file() {
    echo "Compiling $input_file..."
    
    # Check if --arm flag is passed
    if [ "$arch_flag" == "--arm" ]; then
        # Run cargo command with target
        cargo run --target aarch64-apple-darwin -- --file "$input_file"
    elif [ "$arch_flag" == "--x86" ]; then
        # Run cargo command with target
        cargo run --target x86_64-apple-darwin -- --file "$input_file"
    else
        # Run cargo command without target
        cargo run -- --file "$input_file"
    fi
    
    # Check if cargo command was successful
    if [ $? -eq 0 ]; then
        echo "Linking $input_file.bin..."
        
        # Run clang command
        clang -o "$base_name" "$input_file.bin"
        
        if [ $? -eq 0 ]; then
            echo "Success! Executable created: $base_name"
            return 0
        else
            echo "Error: Linking failed"
            return 1
        fi
    else
        echo "Error: Compilation failed"
        return 1
    fi
}

# Handle different commands
case "$command" in
    "compile")
        compile_file "$arm_flag"
        ;;
    "run")
        if compile_file "$arm_flag"; then
            echo "Running $base_name..."
            echo "----------------------------------------"
            ./$base_name
            echo "----------------------------------------"
        else
            echo "Cannot run: compilation failed"
            exit 1
        fi
        ;;
    *)
        echo "Error: Unknown command '$command'"
        show_usage
        ;;
esac
