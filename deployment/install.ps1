param($installPath, $toolsPath, $package, $project)

$file1 = $project.ProjectItems.Item("WinSCP.exe")

# set 'Copy To Output Directory' to 'Copy if newer'
$copyToOutput = $file1.Properties.Item("CopyToOutputDirectory")
$copyToOutput.Value = 2
