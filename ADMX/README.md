# WinSCP Administrative Template (ADMX/ADML)

This package provides a custom Windows Administrative Template for the machine-level WinSCP administration settings as documented at:

https://winscp.net/eng/docs/administration

## Included policies

Under **Computer Configuration > Administrative Templates > WinSCP**:

### Security restrictions
- Prevent users from storing passwords (`DisablePasswordStoring`)
- Prevent opening or editing files from WinSCP (`DisableOpenEdit`)
- Require SSH host keys to be accepted in advance (`DisableAcceptingHostKeys`)
- Force authentication banners to be shown (`ForceBanners`)

### Application defaults
- Configure default automatic update-check period for new users (`DefaultUpdatesPeriod`)
- Default anonymous usage-statistics collection for new users (`DefaultCollectUsage`)

## Registry location

The template follows the current WinSCP administration documentation and writes the values beneath:

`HKLM\SOFTWARE\Wow6432Node\Martin Prikryl\WinSCP 2`

The current WinSCP administration documentation explicitly uses the 32-bit registry view (`Wow6432Node`) on 64-bit Windows, so this template targets that documented path.

## Important behaviour of the two Default* settings

`DefaultUpdatesPeriod` and `DefaultCollectUsage` are defaults, not hard-enforcement controls. They affect users who do not already have corresponding per-user settings. Existing user configuration can override these defaults.

The four security restriction settings are the vendor-documented machine-level restrictions/enforcements.

## Language resources

This package includes both:

- `en-US\WinSCP.adml` - required for Microsoft Intune custom ADMX import
- `en-GB\WinSCP.adml` - for UK-English Local Group Policy and Active Directory Central Store environments

The `en-GB` resource uses the same policy/resource IDs as the `en-US` file, so both languages work with the same `WinSCP.admx`.

## Intune import

1. In the Intune admin center, go to **Devices > Configuration > Import ADMX**.
2. Import `WinSCP.admx` and pair it with `en-US\WinSCP.adml`.
3. Create a **Windows 10 and later** policy using **Imported Administrative templates**.
4. Browse to **WinSCP** and configure the required settings.

At the time this package was produced, Microsoft Intune custom ADMX import supports only `en-US` ADML resources, so **do not use the `en-GB` ADML for the Intune import**. The `en-GB` resource is intended for Windows Local Group Policy and Active Directory.

The template is standalone and declares no external ADMX namespace dependencies.

## Active Directory / Group Policy Central Store

Copy:

- `WinSCP.admx` to `\\<domain>\SYSVOL\<domain>\Policies\PolicyDefinitions\`
- `WinSCP.adml` to the language folder used by your administrators, for example:
  - UK English: `\\<domain>\SYSVOL\<domain>\Policies\PolicyDefinitions\en-GB\`
  - US English: `\\<domain>\SYSVOL\<domain>\Policies\PolicyDefinitions\en-US\`

For a UK-English Windows 11 test machine, copy `WinSCP.admx` to `C:\Windows\PolicyDefinitions\` and the UK resource file to `C:\Windows\PolicyDefinitions\en-GB\WinSCP.adml`. Then run `gpedit.msc`.

For Active Directory, edit a GPO and browse to **Computer Configuration > Policies > Administrative Templates > WinSCP**.

## Unmodifiable saved sites

The administration page also documents the per-session `Special=1` setting for making a saved site harder to accidentally modify or delete. It is **not included as a generic ADMX policy** because each WinSCP site is stored in a dynamically named `Sessions\<SiteName>` subkey, which a normal ADMX setting cannot parameterise as a registry key path. WinSCP also notes that the end user can change the `Special` value, so it is not a security enforcement boundary. If you have a fixed set of centrally provisioned WinSCP site names, a site-specific extension to this template can be generated.

## Suggested security baseline

A security-conscious enterprise baseline could enable:

- Prevent users from storing passwords
- Require SSH host keys to be accepted in advance (only after trusted host keys are provisioned)
- Force authentication banners to be shown

Whether to disable file opening/editing depends on your operational use case because it removes a normal WinSCP workflow.

For update checking and anonymous statistics, decide whether WinSCP itself or your software-management platform owns update/telemetry behaviour. Remember that the `Default*` values do not override an existing user's saved preference.
