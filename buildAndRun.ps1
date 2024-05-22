Param (
[string]$path
)

Write-Host "Restore nuget"
dotnet restore /p:CheckEolTargetFramework=false

If ($lastExitCode -ne "0") {
    Write-Error "Nuget restore is failed"
    exit
}
Write-Host
Write-Host "Validating configs"
dotnet run --project ".\ConfigGenerator\ConfigGenerator.fsproj" $path
If ($lastExitCode -ne "0") {
    Write-Error "Validating configs is failed"
    exit
}
Write-Host

# Write-Host
# Write-Host "Build sln"

# dotnet build /p:CheckEolTargetFramework=false
# If ($lastExitCode -ne "0") {
#     Write-Error "Build sln was failed"
#     exit
# }
