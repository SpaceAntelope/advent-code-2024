[CmdletBinding()]
param (
    [Parameter(Mandatory, Position=0)]
    [string]
    $inputPath
)


Write-Output "digraph {"
Get-Content $inputPath
| Where-Object { $_ -match "AND|XOR|OR" }
| ForEach-Object {$i = 0; $gv = @() } {
    $i++
    $m = [regex]::Match($_, "(?<s1>\w+) (?<op>AND|OR|XOR) (?<s2>\w+) \-\> (?<s3>\w+)")
    $left = $m.groups["s1"].Value
    $right = $m.groups["s2"].Value
    $op = $m.groups["op"].Value
    $out = $m.groups["s3"].Value

    $gate = "${op}_$i"

    $gv += "$left -> $gate"
    $gv += "$right -> $gate"
    $gv += "$gate -> $out"

} { $gv }
| ForEach-Object { write-output "    $_"}

write-output "}"
