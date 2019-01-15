#if !NETSTANDARD
using System;
using System.Runtime.InteropServices;
using System.Security.AccessControl;
using System.Security.Principal;

namespace WinSCP
{
    // All the code to manipulate security object is available in .NET framework,
    // but its API tries to be type-safe and handle-safe, enforcing special implemnentation
    // (to otherwise a generic WinAPI) for each handle type. This is to make sure
    // only correct set of permissions can be set for corresponding object types and
    // mainly that handles do not leak.
    // Hence the AccessRule and NativeObjectSecurity clases are abstract.
    // This is the simplest possible implementation that yet allows us to make use
    // of the existing .NET implementation, sparing necessity to P/Invoke the underlying WinAPI.

    internal class GenericAccessRule : AccessRule
    {
        public GenericAccessRule(IdentityReference identity, int accessMask, AccessControlType type) :
            base(identity, accessMask, false, InheritanceFlags.None, PropagationFlags.None, type)
        {
        }
    }

    internal class GenericSecurity : NativeObjectSecurity
    {
        public GenericSecurity(bool isContainer, ResourceType resType, SafeHandle objectHandle, AccessControlSections sectionsRequested)
            : base(isContainer, resType, objectHandle, sectionsRequested)
        {
        }

        new public void Persist(SafeHandle handle, AccessControlSections includeSections)
        {
            base.Persist(handle, includeSections);
        }

        new public void AddAccessRule(AccessRule rule)
        {
            base.AddAccessRule(rule);
        }

        #region NativeObjectSecurity Abstract Method Overrides

        public override Type AccessRightType
        {
            get { throw new NotImplementedException(); }
        }

        public override AccessRule AccessRuleFactory(System.Security.Principal.IdentityReference identityReference, int accessMask, bool isInherited, InheritanceFlags inheritanceFlags, PropagationFlags propagationFlags, AccessControlType type)
        {
            throw new NotImplementedException();
        }

        public override Type AccessRuleType
        {
            get { return typeof(AccessRule); }
        }

        public override AuditRule AuditRuleFactory(System.Security.Principal.IdentityReference identityReference, int accessMask, bool isInherited, InheritanceFlags inheritanceFlags, PropagationFlags propagationFlags, AuditFlags flags)
        {
            throw new NotImplementedException();
        }

        public override Type AuditRuleType
        {
            get { return typeof(AuditRule); }
        }

        #endregion
    }
}
#endif
