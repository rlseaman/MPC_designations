# mpc_designation_main.awk - Main block for CLI use
# Usage: awk -f mpc_designation.awk -f mpc_designation_main.awk

{
    # Process each line as a designation
    result = convert_simple($0)
    print result
}
