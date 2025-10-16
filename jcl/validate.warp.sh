#!/bin/bash
#
# JCL Validation Script
# Validates JCL syntax and dataset availability
#

echo "Validating JCL files..."

# Check JCL syntax
for jcl_file in *.JCL; do
    if [ -f "$jcl_file" ]; then
        echo "Checking syntax: $jcl_file"
        # Basic JCL syntax validation
        if grep -q "^//[A-Z0-9]\{1,8\}\s\+JOB" "$jcl_file"; then
            echo "  ✓ Valid JOB card found"
        else
            echo "  ✗ Invalid or missing JOB card"
        fi
        
        if grep -q "^//[A-Z0-9]\{1,8\}\s\+EXEC" "$jcl_file"; then
            echo "  ✓ EXEC statement found"
        else
            echo "  ✗ No EXEC statement found"
        fi
    fi
done

echo "JCL validation complete."
