namespace Gitea

open WoofWare.Myriad.Plugins

/// APIError is an api error with a message
[<JsonParse true ; JsonSerialize true>]
type APIError =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "message">]
        Message : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// AccessToken represents an API access token.
[<JsonParse true ; JsonSerialize true>]
type AccessToken =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "scopes">]
        Scopes : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "sha1">]
        Sha1 : string option
        [<System.Text.Json.Serialization.JsonPropertyName "token_last_eight">]
        TokenLastEight : string option
    }

/// ActivityPub type
[<JsonParse true ; JsonSerialize true>]
type ActivityPub =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "@context">]
        Context : string option
    }

/// AddCollaboratorOption options when adding a user as a collaborator of a repository
[<JsonParse true ; JsonSerialize true>]
type AddCollaboratorOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "permission">]
        Permission : string option
    }

/// AddTimeOption options for adding time to an issue
[<JsonParse true ; JsonSerialize true>]
type AddTimeOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "created">]
        Created : string option
        [<System.Text.Json.Serialization.JsonPropertyName "time">]
        Time : int
        [<System.Text.Json.Serialization.JsonPropertyName "user_name">]
        UserName : string option
    }

/// AnnotatedTagObject contains meta information of the tag object
[<JsonParse true ; JsonSerialize true>]
type AnnotatedTagObject =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "sha">]
        Sha : string option
        [<System.Text.Json.Serialization.JsonPropertyName "type">]
        Type : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// Attachment a generic attachment
[<JsonParse true ; JsonSerialize true>]
type Attachment =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "browser_download_url">]
        BrowserDownloadUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "download_count">]
        DownloadCount : int option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "size">]
        Size : int option
        [<System.Text.Json.Serialization.JsonPropertyName "uuid">]
        Uuid : string option
    }

/// BranchProtection represents a branch protection for a repository
[<JsonParse true ; JsonSerialize true>]
type BranchProtection =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "approvals_whitelist_teams">]
        ApprovalsWhitelistTeams : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "approvals_whitelist_username">]
        ApprovalsWhitelistUsername : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "block_on_official_review_requests">]
        BlockOnOfficialReviewRequests : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "block_on_outdated_branch">]
        BlockOnOutdatedBranch : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "block_on_rejected_reviews">]
        BlockOnRejectedReviews : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "branch_name">]
        BranchName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "dismiss_stale_approvals">]
        DismissStaleApprovals : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_approvals_whitelist">]
        EnableApprovalsWhitelist : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_merge_whitelist">]
        EnableMergeWhitelist : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_push">]
        EnablePush : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_push_whitelist">]
        EnablePushWhitelist : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_status_check">]
        EnableStatusCheck : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "merge_whitelist_teams">]
        MergeWhitelistTeams : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "merge_whitelist_usernames">]
        MergeWhitelistUsernames : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "protected_file_patterns">]
        ProtectedFilePatterns : string option
        [<System.Text.Json.Serialization.JsonPropertyName "push_whitelist_deploy_keys">]
        PushWhitelistDeployKeys : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "push_whitelist_teams">]
        PushWhitelistTeams : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "push_whitelist_usernames">]
        PushWhitelistUsernames : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "require_signed_commits">]
        RequireSignedCommits : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "required_approvals">]
        RequiredApprovals : int option
        [<System.Text.Json.Serialization.JsonPropertyName "rule_name">]
        RuleName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "status_check_contexts">]
        StatusCheckContexts : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "unprotected_file_patterns">]
        UnprotectedFilePatterns : string option
        [<System.Text.Json.Serialization.JsonPropertyName "updated_at">]
        UpdatedAt : string option
    }

/// ChangedFile store information about files affected by the pull request
[<JsonParse true ; JsonSerialize true>]
type ChangedFile =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "additions">]
        Additions : int option
        [<System.Text.Json.Serialization.JsonPropertyName "changes">]
        Changes : int option
        [<System.Text.Json.Serialization.JsonPropertyName "contents_url">]
        ContentsUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "deletions">]
        Deletions : int option
        [<System.Text.Json.Serialization.JsonPropertyName "filename">]
        Filename : string option
        [<System.Text.Json.Serialization.JsonPropertyName "html_url">]
        HtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "previous_filename">]
        PreviousFilename : string option
        [<System.Text.Json.Serialization.JsonPropertyName "raw_url">]
        RawUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "status">]
        Status : string option
    }

/// CommitAffectedFiles store information about files affected by the commit
[<JsonParse true ; JsonSerialize true>]
type CommitAffectedFiles =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "filename">]
        Filename : string option
    }

/// CommitDateOptions store dates for GIT_AUTHOR_DATE and GIT_COMMITTER_DATE
[<JsonParse true ; JsonSerialize true>]
type CommitDateOptions =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "author">]
        Author : string option
        [<System.Text.Json.Serialization.JsonPropertyName "committer">]
        Committer : string option
    }

/// CommitMeta contains meta information of a commit in terms of API.
[<JsonParse true ; JsonSerialize true>]
type CommitMeta =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "created">]
        Created : string option
        [<System.Text.Json.Serialization.JsonPropertyName "sha">]
        Sha : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// CommitStats is statistics for a RepoCommit
[<JsonParse true ; JsonSerialize true>]
type CommitStats =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "additions">]
        Additions : int option
        [<System.Text.Json.Serialization.JsonPropertyName "deletions">]
        Deletions : int option
        [<System.Text.Json.Serialization.JsonPropertyName "total">]
        Total : int option
    }

/// CommitUser contains information of a user in the context of a commit.
[<JsonParse true ; JsonSerialize true>]
type CommitUser =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "date">]
        Date : string option
        [<System.Text.Json.Serialization.JsonPropertyName "email">]
        Email : string option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
    }

/// CreateAccessTokenOption options when create access token
[<JsonParse true ; JsonSerialize true>]
type CreateAccessTokenOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string
        [<System.Text.Json.Serialization.JsonPropertyName "scopes">]
        Scopes : string list option
    }

/// CreateBranchProtectionOption options for creating a branch protection
[<JsonParse true ; JsonSerialize true>]
type CreateBranchProtectionOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "approvals_whitelist_teams">]
        ApprovalsWhitelistTeams : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "approvals_whitelist_username">]
        ApprovalsWhitelistUsername : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "block_on_official_review_requests">]
        BlockOnOfficialReviewRequests : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "block_on_outdated_branch">]
        BlockOnOutdatedBranch : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "block_on_rejected_reviews">]
        BlockOnRejectedReviews : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "branch_name">]
        BranchName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "dismiss_stale_approvals">]
        DismissStaleApprovals : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_approvals_whitelist">]
        EnableApprovalsWhitelist : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_merge_whitelist">]
        EnableMergeWhitelist : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_push">]
        EnablePush : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_push_whitelist">]
        EnablePushWhitelist : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_status_check">]
        EnableStatusCheck : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "merge_whitelist_teams">]
        MergeWhitelistTeams : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "merge_whitelist_usernames">]
        MergeWhitelistUsernames : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "protected_file_patterns">]
        ProtectedFilePatterns : string option
        [<System.Text.Json.Serialization.JsonPropertyName "push_whitelist_deploy_keys">]
        PushWhitelistDeployKeys : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "push_whitelist_teams">]
        PushWhitelistTeams : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "push_whitelist_usernames">]
        PushWhitelistUsernames : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "require_signed_commits">]
        RequireSignedCommits : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "required_approvals">]
        RequiredApprovals : int option
        [<System.Text.Json.Serialization.JsonPropertyName "rule_name">]
        RuleName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "status_check_contexts">]
        StatusCheckContexts : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "unprotected_file_patterns">]
        UnprotectedFilePatterns : string option
    }

/// CreateBranchRepoOption options when creating a branch in a repository
[<JsonParse true ; JsonSerialize true>]
type CreateBranchRepoOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "new_branch_name">]
        NewBranchName : string
        [<System.Text.Json.Serialization.JsonPropertyName "old_branch_name">]
        OldBranchName : string option
    }

/// CreateEmailOption options when creating email addresses
[<JsonParse true ; JsonSerialize true>]
type CreateEmailOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "emails">]
        Emails : string list option
    }

/// CreateForkOption options for creating a fork
[<JsonParse true ; JsonSerialize true>]
type CreateForkOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "organization">]
        Organization : string option
    }

/// CreateGPGKeyOption options create user GPG key
[<JsonParse true ; JsonSerialize true>]
type CreateGPGKeyOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "armored_public_key">]
        ArmoredPublicKey : string
        [<System.Text.Json.Serialization.JsonPropertyName "armored_signature">]
        ArmoredSignature : string option
    }

/// CreateHookOptionConfig has all config options in it
/// required are "content_type" and "url" Required
[<JsonParse true ; JsonSerialize true>]
type CreateHookOptionConfig =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, string>
    }

/// CreateIssueCommentOption options for creating a comment on an issue
[<JsonParse true ; JsonSerialize true>]
type CreateIssueCommentOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string
    }

/// CreateIssueOption options to create one issue
[<JsonParse true ; JsonSerialize true>]
type CreateIssueOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "assignee">]
        Assignee : string option
        [<System.Text.Json.Serialization.JsonPropertyName "assignees">]
        Assignees : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "closed">]
        Closed : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "due_date">]
        DueDate : string option
        [<System.Text.Json.Serialization.JsonPropertyName "labels">]
        Labels : int list option
        [<System.Text.Json.Serialization.JsonPropertyName "milestone">]
        Milestone : int option
        [<System.Text.Json.Serialization.JsonPropertyName "ref">]
        Ref : string option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string
    }

/// CreateKeyOption options when creating a key
[<JsonParse true ; JsonSerialize true>]
type CreateKeyOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "key">]
        Key : string
        [<System.Text.Json.Serialization.JsonPropertyName "read_only">]
        ReadOnly : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string
    }

/// CreateLabelOption options for creating a label
[<JsonParse true ; JsonSerialize true>]
type CreateLabelOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "color">]
        Color : string
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "exclusive">]
        Exclusive : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string
    }

/// CreateMilestoneOption options for creating a milestone
[<JsonParse true ; JsonSerialize true>]
type CreateMilestoneOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "due_on">]
        DueOn : string option
        [<System.Text.Json.Serialization.JsonPropertyName "state">]
        State : string option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string option
    }

/// CreateOAuth2ApplicationOptions holds options to create an oauth2 application
[<JsonParse true ; JsonSerialize true>]
type CreateOAuth2ApplicationOptions =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "confidential_client">]
        ConfidentialClient : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "redirect_uris">]
        RedirectUris : string list option
    }

/// CreateOrgOption options for creating an organization
[<JsonParse true ; JsonSerialize true>]
type CreateOrgOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "full_name">]
        FullName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "location">]
        Location : string option
        [<System.Text.Json.Serialization.JsonPropertyName "repo_admin_change_team_access">]
        RepoAdminChangeTeamAccess : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "username">]
        Username : string
        [<System.Text.Json.Serialization.JsonPropertyName "visibility">]
        Visibility : string option
        [<System.Text.Json.Serialization.JsonPropertyName "website">]
        Website : string option
    }

/// CreatePullRequestOption options when creating a pull request
[<JsonParse true ; JsonSerialize true>]
type CreatePullRequestOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "assignee">]
        Assignee : string option
        [<System.Text.Json.Serialization.JsonPropertyName "assignees">]
        Assignees : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "base">]
        Base : string option
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "due_date">]
        DueDate : string option
        [<System.Text.Json.Serialization.JsonPropertyName "head">]
        Head : string option
        [<System.Text.Json.Serialization.JsonPropertyName "labels">]
        Labels : int list option
        [<System.Text.Json.Serialization.JsonPropertyName "milestone">]
        Milestone : int option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string option
    }

/// CreatePullReviewComment represent a review comment for creation api
[<JsonParse true ; JsonSerialize true>]
type CreatePullReviewComment =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "new_position">]
        NewPosition : int option
        [<System.Text.Json.Serialization.JsonPropertyName "old_position">]
        OldPosition : int option
        [<System.Text.Json.Serialization.JsonPropertyName "path">]
        Path : string option
    }

/// CreatePushMirrorOption represents need information to create a push mirror of a repository.
[<JsonParse true ; JsonSerialize true>]
type CreatePushMirrorOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "interval">]
        Interval : string option
        [<System.Text.Json.Serialization.JsonPropertyName "remote_address">]
        RemoteAddress : string option
        [<System.Text.Json.Serialization.JsonPropertyName "remote_password">]
        RemotePassword : string option
        [<System.Text.Json.Serialization.JsonPropertyName "remote_username">]
        RemoteUsername : string option
        [<System.Text.Json.Serialization.JsonPropertyName "sync_on_commit">]
        SyncOnCommit : bool option
    }

/// CreateReleaseOption options when creating a release
[<JsonParse true ; JsonSerialize true>]
type CreateReleaseOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "draft">]
        Draft : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "prerelease">]
        Prerelease : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "tag_name">]
        TagName : string
        [<System.Text.Json.Serialization.JsonPropertyName "target_commitish">]
        TargetCommitish : string option
    }

/// CreateRepoOption options when creating repository
[<JsonParse true ; JsonSerialize true>]
type CreateRepoOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "auto_init">]
        AutoInit : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "default_branch">]
        DefaultBranch : string option
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "gitignores">]
        Gitignores : string option
        [<System.Text.Json.Serialization.JsonPropertyName "issue_labels">]
        IssueLabels : string option
        [<System.Text.Json.Serialization.JsonPropertyName "license">]
        License : string option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string
        [<System.Text.Json.Serialization.JsonPropertyName "private">]
        Private : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "readme">]
        Readme : string option
        [<System.Text.Json.Serialization.JsonPropertyName "template">]
        Template : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "trust_model">]
        TrustModel : string option
    }

/// CreateStatusOption holds the information needed to create a new CommitStatus for a Commit
[<JsonParse true ; JsonSerialize true>]
type CreateStatusOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "context">]
        Context : string option
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "state">]
        State : string option
        [<System.Text.Json.Serialization.JsonPropertyName "target_url">]
        TargetUrl : string option
    }

/// CreateTagOption options when creating a tag
[<JsonParse true ; JsonSerialize true>]
type CreateTagOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "message">]
        Message : string option
        [<System.Text.Json.Serialization.JsonPropertyName "tag_name">]
        TagName : string
        [<System.Text.Json.Serialization.JsonPropertyName "target">]
        Target : string option
    }

[<JsonParse true ; JsonSerialize true>]
type Type1 =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, string>
    }

/// CreateTeamOption options for creating a team
[<JsonParse true ; JsonSerialize true>]
type CreateTeamOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "can_create_org_repo">]
        CanCreateOrgRepo : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "includes_all_repositories">]
        IncludesAllRepositories : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string
        [<System.Text.Json.Serialization.JsonPropertyName "permission">]
        Permission : string option
        [<System.Text.Json.Serialization.JsonPropertyName "units">]
        Units : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "units_map">]
        UnitsMap : Type1 option
    }

/// CreateUserOption create user options
[<JsonParse true ; JsonSerialize true>]
type CreateUserOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "email">]
        Email : string
        [<System.Text.Json.Serialization.JsonPropertyName "full_name">]
        FullName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "login_name">]
        LoginName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "must_change_password">]
        MustChangePassword : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "password">]
        Password : string
        [<System.Text.Json.Serialization.JsonPropertyName "restricted">]
        Restricted : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "send_notify">]
        SendNotify : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "source_id">]
        SourceId : int option
        [<System.Text.Json.Serialization.JsonPropertyName "username">]
        Username : string
        [<System.Text.Json.Serialization.JsonPropertyName "visibility">]
        Visibility : string option
    }

/// CreateWikiPageOptions form for creating wiki
[<JsonParse true ; JsonSerialize true>]
type CreateWikiPageOptions =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "content_base64">]
        ContentBase64 : string option
        [<System.Text.Json.Serialization.JsonPropertyName "message">]
        Message : string option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string option
    }

/// Cron represents a Cron task
[<JsonParse true ; JsonSerialize true>]
type Cron =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "exec_times">]
        ExecTimes : int option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "next">]
        Next : string option
        [<System.Text.Json.Serialization.JsonPropertyName "prev">]
        Prev : string option
        [<System.Text.Json.Serialization.JsonPropertyName "schedule">]
        Schedule : string option
    }

/// DeleteEmailOption options when deleting email addresses
[<JsonParse true ; JsonSerialize true>]
type DeleteEmailOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "emails">]
        Emails : string list option
    }

/// DismissPullReviewOptions are options to dismiss a pull review
[<JsonParse true ; JsonSerialize true>]
type DismissPullReviewOptions =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "message">]
        Message : string option
        [<System.Text.Json.Serialization.JsonPropertyName "priors">]
        Priors : bool option
    }

/// EditAttachmentOptions options for editing attachments
[<JsonParse true ; JsonSerialize true>]
type EditAttachmentOptions =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
    }

/// EditBranchProtectionOption options for editing a branch protection
[<JsonParse true ; JsonSerialize true>]
type EditBranchProtectionOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "approvals_whitelist_teams">]
        ApprovalsWhitelistTeams : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "approvals_whitelist_username">]
        ApprovalsWhitelistUsername : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "block_on_official_review_requests">]
        BlockOnOfficialReviewRequests : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "block_on_outdated_branch">]
        BlockOnOutdatedBranch : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "block_on_rejected_reviews">]
        BlockOnRejectedReviews : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "dismiss_stale_approvals">]
        DismissStaleApprovals : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_approvals_whitelist">]
        EnableApprovalsWhitelist : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_merge_whitelist">]
        EnableMergeWhitelist : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_push">]
        EnablePush : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_push_whitelist">]
        EnablePushWhitelist : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_status_check">]
        EnableStatusCheck : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "merge_whitelist_teams">]
        MergeWhitelistTeams : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "merge_whitelist_usernames">]
        MergeWhitelistUsernames : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "protected_file_patterns">]
        ProtectedFilePatterns : string option
        [<System.Text.Json.Serialization.JsonPropertyName "push_whitelist_deploy_keys">]
        PushWhitelistDeployKeys : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "push_whitelist_teams">]
        PushWhitelistTeams : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "push_whitelist_usernames">]
        PushWhitelistUsernames : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "require_signed_commits">]
        RequireSignedCommits : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "required_approvals">]
        RequiredApprovals : int option
        [<System.Text.Json.Serialization.JsonPropertyName "status_check_contexts">]
        StatusCheckContexts : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "unprotected_file_patterns">]
        UnprotectedFilePatterns : string option
    }

/// EditDeadlineOption options for creating a deadline
[<JsonParse true ; JsonSerialize true>]
type EditDeadlineOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "due_date">]
        DueDate : string
    }

/// EditGitHookOption options when modifying one Git hook
[<JsonParse true ; JsonSerialize true>]
type EditGitHookOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "content">]
        Content : string option
    }

[<JsonParse true ; JsonSerialize true>]
type Type2 =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, string>
    }

/// EditHookOption options when modify one hook
[<JsonParse true ; JsonSerialize true>]
type EditHookOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "active">]
        Active : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "authorization_header">]
        AuthorizationHeader : string option
        [<System.Text.Json.Serialization.JsonPropertyName "branch_filter">]
        BranchFilter : string option
        [<System.Text.Json.Serialization.JsonPropertyName "config">]
        Config : Type2 option
        [<System.Text.Json.Serialization.JsonPropertyName "events">]
        Events : string list option
    }

/// EditIssueCommentOption options for editing a comment
[<JsonParse true ; JsonSerialize true>]
type EditIssueCommentOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string
    }

/// EditIssueOption options for editing an issue
[<JsonParse true ; JsonSerialize true>]
type EditIssueOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "assignee">]
        Assignee : string option
        [<System.Text.Json.Serialization.JsonPropertyName "assignees">]
        Assignees : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "due_date">]
        DueDate : string option
        [<System.Text.Json.Serialization.JsonPropertyName "milestone">]
        Milestone : int option
        [<System.Text.Json.Serialization.JsonPropertyName "ref">]
        Ref : string option
        [<System.Text.Json.Serialization.JsonPropertyName "state">]
        State : string option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string option
        [<System.Text.Json.Serialization.JsonPropertyName "unset_due_date">]
        UnsetDueDate : bool option
    }

/// EditLabelOption options for editing a label
[<JsonParse true ; JsonSerialize true>]
type EditLabelOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "color">]
        Color : string option
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "exclusive">]
        Exclusive : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
    }

/// EditMilestoneOption options for editing a milestone
[<JsonParse true ; JsonSerialize true>]
type EditMilestoneOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "due_on">]
        DueOn : string option
        [<System.Text.Json.Serialization.JsonPropertyName "state">]
        State : string option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string option
    }

/// EditOrgOption options for editing an organization
[<JsonParse true ; JsonSerialize true>]
type EditOrgOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "full_name">]
        FullName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "location">]
        Location : string option
        [<System.Text.Json.Serialization.JsonPropertyName "repo_admin_change_team_access">]
        RepoAdminChangeTeamAccess : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "visibility">]
        Visibility : string option
        [<System.Text.Json.Serialization.JsonPropertyName "website">]
        Website : string option
    }

/// EditPullRequestOption options when modify pull request
[<JsonParse true ; JsonSerialize true>]
type EditPullRequestOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "allow_maintainer_edit">]
        AllowMaintainerEdit : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "assignee">]
        Assignee : string option
        [<System.Text.Json.Serialization.JsonPropertyName "assignees">]
        Assignees : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "base">]
        Base : string option
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "due_date">]
        DueDate : string option
        [<System.Text.Json.Serialization.JsonPropertyName "labels">]
        Labels : int list option
        [<System.Text.Json.Serialization.JsonPropertyName "milestone">]
        Milestone : int option
        [<System.Text.Json.Serialization.JsonPropertyName "state">]
        State : string option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string option
        [<System.Text.Json.Serialization.JsonPropertyName "unset_due_date">]
        UnsetDueDate : bool option
    }

/// EditReactionOption contain the reaction type
[<JsonParse true ; JsonSerialize true>]
type EditReactionOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "content">]
        Content : string option
    }

/// EditReleaseOption options when editing a release
[<JsonParse true ; JsonSerialize true>]
type EditReleaseOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "draft">]
        Draft : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "prerelease">]
        Prerelease : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "tag_name">]
        TagName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "target_commitish">]
        TargetCommitish : string option
    }

[<JsonParse true ; JsonSerialize true>]
type Type3 =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, string>
    }

/// EditTeamOption options for editing a team
[<JsonParse true ; JsonSerialize true>]
type EditTeamOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "can_create_org_repo">]
        CanCreateOrgRepo : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "includes_all_repositories">]
        IncludesAllRepositories : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string
        [<System.Text.Json.Serialization.JsonPropertyName "permission">]
        Permission : string option
        [<System.Text.Json.Serialization.JsonPropertyName "units">]
        Units : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "units_map">]
        UnitsMap : Type3 option
    }

/// EditUserOption edit user options
[<JsonParse true ; JsonSerialize true>]
type EditUserOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "active">]
        Active : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "admin">]
        Admin : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "allow_create_organization">]
        AllowCreateOrganization : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "allow_git_hook">]
        AllowGitHook : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "allow_import_local">]
        AllowImportLocal : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "email">]
        Email : string option
        [<System.Text.Json.Serialization.JsonPropertyName "full_name">]
        FullName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "location">]
        Location : string option
        [<System.Text.Json.Serialization.JsonPropertyName "login_name">]
        LoginName : string
        [<System.Text.Json.Serialization.JsonPropertyName "max_repo_creation">]
        MaxRepoCreation : int option
        [<System.Text.Json.Serialization.JsonPropertyName "must_change_password">]
        MustChangePassword : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "password">]
        Password : string option
        [<System.Text.Json.Serialization.JsonPropertyName "prohibit_login">]
        ProhibitLogin : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "restricted">]
        Restricted : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "source_id">]
        SourceId : int
        [<System.Text.Json.Serialization.JsonPropertyName "visibility">]
        Visibility : string option
        [<System.Text.Json.Serialization.JsonPropertyName "website">]
        Website : string option
    }

/// Email an email address belonging to a user
[<JsonParse true ; JsonSerialize true>]
type Email =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "email">]
        Email : string option
        [<System.Text.Json.Serialization.JsonPropertyName "primary">]
        Primary : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "verified">]
        Verified : bool option
    }

/// ExternalTracker represents settings for external tracker
[<JsonParse true ; JsonSerialize true>]
type ExternalTracker =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "external_tracker_format">]
        ExternalTrackerFormat : string option
        [<System.Text.Json.Serialization.JsonPropertyName "external_tracker_regexp_pattern">]
        ExternalTrackerRegexpPattern : string option
        [<System.Text.Json.Serialization.JsonPropertyName "external_tracker_style">]
        ExternalTrackerStyle : string option
        [<System.Text.Json.Serialization.JsonPropertyName "external_tracker_url">]
        ExternalTrackerUrl : string option
    }

/// ExternalWiki represents setting for external wiki
[<JsonParse true ; JsonSerialize true>]
type ExternalWiki =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "external_wiki_url">]
        ExternalWikiUrl : string option
    }

/// FileCommitResponse contains information generated from a Git commit for a repo's file.
[<JsonParse true ; JsonSerialize true>]
type FileCommitResponse =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "author">]
        Author : CommitUser option
        [<System.Text.Json.Serialization.JsonPropertyName "committer">]
        Committer : CommitUser option
        [<System.Text.Json.Serialization.JsonPropertyName "created">]
        Created : string option
        [<System.Text.Json.Serialization.JsonPropertyName "html_url">]
        HtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "message">]
        Message : string option
        [<System.Text.Json.Serialization.JsonPropertyName "parents">]
        Parents : CommitMeta list option
        [<System.Text.Json.Serialization.JsonPropertyName "sha">]
        Sha : string option
        [<System.Text.Json.Serialization.JsonPropertyName "tree">]
        Tree : CommitMeta option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// FileLinksResponse contains the links for a repo's file
[<JsonParse true ; JsonSerialize true>]
type FileLinksResponse =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "git">]
        Git : string option
        [<System.Text.Json.Serialization.JsonPropertyName "html">]
        Html : string option
        [<System.Text.Json.Serialization.JsonPropertyName "self">]
        Self : string option
    }

/// GPGKeyEmail an email attached to a GPGKey
[<JsonParse true ; JsonSerialize true>]
type GPGKeyEmail =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "email">]
        Email : string option
        [<System.Text.Json.Serialization.JsonPropertyName "verified">]
        Verified : bool option
    }

/// GeneralAPISettings contains global api settings exposed by it
[<JsonParse true ; JsonSerialize true>]
type GeneralAPISettings =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "default_git_trees_per_page">]
        DefaultGitTreesPerPage : int option
        [<System.Text.Json.Serialization.JsonPropertyName "default_max_blob_size">]
        DefaultMaxBlobSize : int option
        [<System.Text.Json.Serialization.JsonPropertyName "default_paging_num">]
        DefaultPagingNum : int option
        [<System.Text.Json.Serialization.JsonPropertyName "max_response_items">]
        MaxResponseItems : int option
    }

/// GeneralAttachmentSettings contains global Attachment settings exposed by API
[<JsonParse true ; JsonSerialize true>]
type GeneralAttachmentSettings =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "allowed_types">]
        AllowedTypes : string option
        [<System.Text.Json.Serialization.JsonPropertyName "enabled">]
        Enabled : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "max_files">]
        MaxFiles : int option
        [<System.Text.Json.Serialization.JsonPropertyName "max_size">]
        MaxSize : int option
    }

/// GeneralRepoSettings contains global repository settings exposed by API
[<JsonParse true ; JsonSerialize true>]
type GeneralRepoSettings =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "http_git_disabled">]
        HttpGitDisabled : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "lfs_disabled">]
        LfsDisabled : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "migrations_disabled">]
        MigrationsDisabled : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "mirrors_disabled">]
        MirrorsDisabled : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "stars_disabled">]
        StarsDisabled : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "time_tracking_disabled">]
        TimeTrackingDisabled : bool option
    }

/// GeneralUISettings contains global ui settings exposed by API
[<JsonParse true ; JsonSerialize true>]
type GeneralUISettings =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "allowed_reactions">]
        AllowedReactions : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "custom_emojis">]
        CustomEmojis : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "default_theme">]
        DefaultTheme : string option
    }

/// GenerateRepoOption options when creating repository using a template
[<JsonParse true ; JsonSerialize true>]
type GenerateRepoOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "avatar">]
        Avatar : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "default_branch">]
        DefaultBranch : string option
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "git_content">]
        GitContent : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "git_hooks">]
        GitHooks : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "labels">]
        Labels : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string
        [<System.Text.Json.Serialization.JsonPropertyName "owner">]
        Owner : string
        [<System.Text.Json.Serialization.JsonPropertyName "private">]
        Private : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "topics">]
        Topics : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "webhooks">]
        Webhooks : bool option
    }

/// GitBlobResponse represents a git blob
[<JsonParse true ; JsonSerialize true>]
type GitBlobResponse =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "content">]
        Content : string option
        [<System.Text.Json.Serialization.JsonPropertyName "encoding">]
        Encoding : string option
        [<System.Text.Json.Serialization.JsonPropertyName "sha">]
        Sha : string option
        [<System.Text.Json.Serialization.JsonPropertyName "size">]
        Size : int option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// GitEntry represents a git tree
[<JsonParse true ; JsonSerialize true>]
type GitEntry =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "mode">]
        Mode : string option
        [<System.Text.Json.Serialization.JsonPropertyName "path">]
        Path : string option
        [<System.Text.Json.Serialization.JsonPropertyName "sha">]
        Sha : string option
        [<System.Text.Json.Serialization.JsonPropertyName "size">]
        Size : int option
        [<System.Text.Json.Serialization.JsonPropertyName "type">]
        Type : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// GitHook represents a Git repository hook
[<JsonParse true ; JsonSerialize true>]
type GitHook =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "content">]
        Content : string option
        [<System.Text.Json.Serialization.JsonPropertyName "is_active">]
        IsActive : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
    }

/// GitObject represents a Git object.
[<JsonParse true ; JsonSerialize true>]
type GitObject =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "sha">]
        Sha : string option
        [<System.Text.Json.Serialization.JsonPropertyName "type">]
        Type : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// GitTreeResponse returns a git tree
[<JsonParse true ; JsonSerialize true>]
type GitTreeResponse =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "page">]
        Page : int option
        [<System.Text.Json.Serialization.JsonPropertyName "sha">]
        Sha : string option
        [<System.Text.Json.Serialization.JsonPropertyName "total_count">]
        TotalCount : int option
        [<System.Text.Json.Serialization.JsonPropertyName "tree">]
        Tree : GitEntry list option
        [<System.Text.Json.Serialization.JsonPropertyName "truncated">]
        Truncated : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

[<JsonParse true ; JsonSerialize true>]
type Type4 =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, string>
    }

/// Hook a hook is a web hook when one repository changed
[<JsonParse true ; JsonSerialize true>]
type Hook =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "active">]
        Active : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "authorization_header">]
        AuthorizationHeader : string option
        [<System.Text.Json.Serialization.JsonPropertyName "config">]
        Config : Type4 option
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "events">]
        Events : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "type">]
        Type : string option
        [<System.Text.Json.Serialization.JsonPropertyName "updated_at">]
        UpdatedAt : string option
    }

/// Identity for a person's identity like an author or committer
[<JsonParse true ; JsonSerialize true>]
type Identity =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "email">]
        Email : string option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
    }

/// InternalTracker represents settings for internal tracker
[<JsonParse true ; JsonSerialize true>]
type InternalTracker =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "allow_only_contributors_to_track_time">]
        AllowOnlyContributorsToTrackTime : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_issue_dependencies">]
        EnableIssueDependencies : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_time_tracker">]
        EnableTimeTracker : bool option
    }

/// IssueDeadline represents an issue deadline
[<JsonParse true ; JsonSerialize true>]
type IssueDeadline =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "due_date">]
        DueDate : string option
    }

[<JsonParse true ; JsonSerialize true>]
type Type5 =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, unit>
    }

[<JsonParse true ; JsonSerialize true>]
type Type6 =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, unit>
    }

/// IssueLabelsOption a collection of labels
[<JsonParse true ; JsonSerialize true>]
type IssueLabelsOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "labels">]
        Labels : int list option
    }

/// Label a label to an issue or a pr
[<JsonParse true ; JsonSerialize true>]
type Label =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "color">]
        Color : string option
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "exclusive">]
        Exclusive : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// MarkdownOption markdown options
[<JsonParse true ; JsonSerialize true>]
type MarkdownOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "Context">]
        Context : string option
        [<System.Text.Json.Serialization.JsonPropertyName "Mode">]
        Mode : string option
        [<System.Text.Json.Serialization.JsonPropertyName "Text">]
        Text : string option
        [<System.Text.Json.Serialization.JsonPropertyName "Wiki">]
        Wiki : bool option
    }

/// MergePullRequestForm form for merging Pull Request
[<JsonParse true ; JsonSerialize true>]
type MergePullRequestOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "Do">]
        Do : string
        [<System.Text.Json.Serialization.JsonPropertyName "MergeCommitID">]
        MergeCommitID : string option
        [<System.Text.Json.Serialization.JsonPropertyName "MergeMessageField">]
        MergeMessageField : string option
        [<System.Text.Json.Serialization.JsonPropertyName "MergeTitleField">]
        MergeTitleField : string option
        [<System.Text.Json.Serialization.JsonPropertyName "delete_branch_after_merge">]
        DeleteBranchAfterMerge : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "force_merge">]
        ForceMerge : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "head_commit_id">]
        HeadCommitId : string option
        [<System.Text.Json.Serialization.JsonPropertyName "merge_when_checks_succeed">]
        MergeWhenChecksSucceed : bool option
    }

/// MigrateRepoOptions options for migrating repository's
/// this is used to interact with api v1
[<JsonParse true ; JsonSerialize true>]
type MigrateRepoOptions =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "auth_password">]
        AuthPassword : string option
        [<System.Text.Json.Serialization.JsonPropertyName "auth_token">]
        AuthToken : string option
        [<System.Text.Json.Serialization.JsonPropertyName "auth_username">]
        AuthUsername : string option
        [<System.Text.Json.Serialization.JsonPropertyName "clone_addr">]
        CloneAddr : string
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "issues">]
        Issues : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "labels">]
        Labels : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "lfs">]
        Lfs : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "lfs_endpoint">]
        LfsEndpoint : string option
        [<System.Text.Json.Serialization.JsonPropertyName "milestones">]
        Milestones : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "mirror">]
        Mirror : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "mirror_interval">]
        MirrorInterval : string option
        [<System.Text.Json.Serialization.JsonPropertyName "private">]
        Private : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "pull_requests">]
        PullRequests : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "releases">]
        Releases : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "repo_name">]
        RepoName : string
        [<System.Text.Json.Serialization.JsonPropertyName "repo_owner">]
        RepoOwner : string option
        [<System.Text.Json.Serialization.JsonPropertyName "service">]
        Service : string option
        [<System.Text.Json.Serialization.JsonPropertyName "uid">]
        Uid : int option
        [<System.Text.Json.Serialization.JsonPropertyName "wiki">]
        Wiki : bool option
    }

[<JsonParse true ; JsonSerialize true>]
type Type7 =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
    }

/// NodeInfoServices contains the third party sites this server can connect to via their application API
[<JsonParse true ; JsonSerialize true>]
type NodeInfoServices =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "inbound">]
        Inbound : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "outbound">]
        Outbound : string list option
    }

/// NodeInfoSoftware contains Metadata about server software in use
[<JsonParse true ; JsonSerialize true>]
type NodeInfoSoftware =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "homepage">]
        Homepage : string option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "repository">]
        Repository : string option
        [<System.Text.Json.Serialization.JsonPropertyName "version">]
        Version : string option
    }

/// NodeInfoUsageUsers contains statistics about the users of this server
[<JsonParse true ; JsonSerialize true>]
type NodeInfoUsageUsers =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "activeHalfyear">]
        ActiveHalfyear : int option
        [<System.Text.Json.Serialization.JsonPropertyName "activeMonth">]
        ActiveMonth : int option
        [<System.Text.Json.Serialization.JsonPropertyName "total">]
        Total : int option
    }

/// NotificationCount number of unread notifications
[<JsonParse true ; JsonSerialize true>]
type NotificationCount =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "new">]
        New : int option
    }

/// OAuth2Application represents an OAuth2 application.
[<JsonParse true ; JsonSerialize true>]
type OAuth2Application =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "client_id">]
        ClientId : string option
        [<System.Text.Json.Serialization.JsonPropertyName "client_secret">]
        ClientSecret : string option
        [<System.Text.Json.Serialization.JsonPropertyName "confidential_client">]
        ConfidentialClient : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "created">]
        Created : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "redirect_uris">]
        RedirectUris : string list option
    }

/// Organization represents an organization
[<JsonParse true ; JsonSerialize true>]
type Organization =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "avatar_url">]
        AvatarUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "full_name">]
        FullName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "location">]
        Location : string option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "repo_admin_change_team_access">]
        RepoAdminChangeTeamAccess : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "username">]
        Username : string option
        [<System.Text.Json.Serialization.JsonPropertyName "visibility">]
        Visibility : string option
        [<System.Text.Json.Serialization.JsonPropertyName "website">]
        Website : string option
    }

/// OrganizationPermissions list different users permissions on an organization
[<JsonParse true ; JsonSerialize true>]
type OrganizationPermissions =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "can_create_repository">]
        CanCreateRepository : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "can_read">]
        CanRead : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "can_write">]
        CanWrite : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "is_admin">]
        IsAdmin : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "is_owner">]
        IsOwner : bool option
    }

/// PackageFile represents a package file
[<JsonParse true ; JsonSerialize true>]
type PackageFile =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "Size">]
        Size : int option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "md5">]
        Md5 : string option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "sha1">]
        Sha1 : string option
        [<System.Text.Json.Serialization.JsonPropertyName "sha256">]
        Sha256 : string option
        [<System.Text.Json.Serialization.JsonPropertyName "sha512">]
        Sha512 : string option
    }

/// PayloadUser represents the author or committer of a commit
[<JsonParse true ; JsonSerialize true>]
type PayloadUser =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "email">]
        Email : string option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "username">]
        Username : string option
    }

/// Permission represents a set of permissions
[<JsonParse true ; JsonSerialize true>]
type Permission =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "admin">]
        Admin : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "pull">]
        Pull : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "push">]
        Push : bool option
    }

/// PullRequestMeta PR info if an issue is a PR
[<JsonParse true ; JsonSerialize true>]
type PullRequestMeta =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "merged">]
        Merged : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "merged_at">]
        MergedAt : string option
    }

/// PullReviewRequestOptions are options to add or remove pull review requests
[<JsonParse true ; JsonSerialize true>]
type PullReviewRequestOptions =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "reviewers">]
        Reviewers : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "team_reviewers">]
        TeamReviewers : string list option
    }

/// PushMirror represents information of a push mirror
[<JsonParse true ; JsonSerialize true>]
type PushMirror =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "created">]
        Created : string option
        [<System.Text.Json.Serialization.JsonPropertyName "interval">]
        Interval : string option
        [<System.Text.Json.Serialization.JsonPropertyName "last_error">]
        LastError : string option
        [<System.Text.Json.Serialization.JsonPropertyName "last_update">]
        LastUpdate : string option
        [<System.Text.Json.Serialization.JsonPropertyName "remote_address">]
        RemoteAddress : string option
        [<System.Text.Json.Serialization.JsonPropertyName "remote_name">]
        RemoteName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "repo_name">]
        RepoName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "sync_on_commit">]
        SyncOnCommit : bool option
    }

/// Reference represents a Git reference.
[<JsonParse true ; JsonSerialize true>]
type Reference =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "object">]
        Object : GitObject option
        [<System.Text.Json.Serialization.JsonPropertyName "ref">]
        Ref : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// RepoTopicOptions a collection of repo topic names
[<JsonParse true ; JsonSerialize true>]
type RepoTopicOptions =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "topics">]
        Topics : string list option
    }

/// RepositoryMeta basic repository information
[<JsonParse true ; JsonSerialize true>]
type RepositoryMeta =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "full_name">]
        FullName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "owner">]
        Owner : string option
    }

/// ServerVersion wraps the version of the server
[<JsonParse true ; JsonSerialize true>]
type ServerVersion =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "version">]
        Version : string option
    }

/// StopWatch represent a running stopwatch
[<JsonParse true ; JsonSerialize true>]
type StopWatch =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "created">]
        Created : string option
        [<System.Text.Json.Serialization.JsonPropertyName "duration">]
        Duration : string option
        [<System.Text.Json.Serialization.JsonPropertyName "issue_index">]
        IssueIndex : int option
        [<System.Text.Json.Serialization.JsonPropertyName "issue_title">]
        IssueTitle : string option
        [<System.Text.Json.Serialization.JsonPropertyName "repo_name">]
        RepoName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "repo_owner_name">]
        RepoOwnerName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "seconds">]
        Seconds : int option
    }

/// SubmitPullReviewOptions are options to submit a pending pull review
[<JsonParse true ; JsonSerialize true>]
type SubmitPullReviewOptions =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "event">]
        Event : string option
    }

/// Tag represents a repository tag
[<JsonParse true ; JsonSerialize true>]
type Tag =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "commit">]
        Commit : CommitMeta option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : string option
        [<System.Text.Json.Serialization.JsonPropertyName "message">]
        Message : string option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "tarball_url">]
        TarballUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "zipball_url">]
        ZipballUrl : string option
    }

[<JsonParse true ; JsonSerialize true>]
type Type8 =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, string>
    }

/// Team represents a team in an organization
[<JsonParse true ; JsonSerialize true>]
type Team =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "can_create_org_repo">]
        CanCreateOrgRepo : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "includes_all_repositories">]
        IncludesAllRepositories : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "organization">]
        Organization : Organization option
        [<System.Text.Json.Serialization.JsonPropertyName "permission">]
        Permission : string option
        [<System.Text.Json.Serialization.JsonPropertyName "units">]
        Units : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "units_map">]
        UnitsMap : Type8 option
    }

/// TopicName a list of repo topic names
[<JsonParse true ; JsonSerialize true>]
type TopicName =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "topics">]
        Topics : string list option
    }

/// TopicResponse for returning topics
[<JsonParse true ; JsonSerialize true>]
type TopicResponse =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "created">]
        Created : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "repo_count">]
        RepoCount : int option
        [<System.Text.Json.Serialization.JsonPropertyName "topic_name">]
        TopicName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "updated">]
        Updated : string option
    }

/// TransferRepoOption options when transfer a repository's ownership
[<JsonParse true ; JsonSerialize true>]
type TransferRepoOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "new_owner">]
        NewOwner : string
        [<System.Text.Json.Serialization.JsonPropertyName "team_ids">]
        TeamIds : int list option
    }

/// UpdateFileOptions options for updating files
/// Note: `author` and `committer` are optional (if only one is given, it will be used for the other, otherwise the authenticated user will be used)
[<JsonParse true ; JsonSerialize true>]
type UpdateFileOptions =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "author">]
        Author : Identity option
        [<System.Text.Json.Serialization.JsonPropertyName "branch">]
        Branch : string option
        [<System.Text.Json.Serialization.JsonPropertyName "committer">]
        Committer : Identity option
        [<System.Text.Json.Serialization.JsonPropertyName "content">]
        Content : string
        [<System.Text.Json.Serialization.JsonPropertyName "dates">]
        Dates : CommitDateOptions option
        [<System.Text.Json.Serialization.JsonPropertyName "from_path">]
        FromPath : string option
        [<System.Text.Json.Serialization.JsonPropertyName "message">]
        Message : string option
        [<System.Text.Json.Serialization.JsonPropertyName "new_branch">]
        NewBranch : string option
        [<System.Text.Json.Serialization.JsonPropertyName "sha">]
        Sha : string
        [<System.Text.Json.Serialization.JsonPropertyName "signoff">]
        Signoff : bool option
    }

/// User represents a user
[<JsonParse true ; JsonSerialize true>]
type User =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "active">]
        Active : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "avatar_url">]
        AvatarUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "created">]
        Created : string option
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "email">]
        Email : string option
        [<System.Text.Json.Serialization.JsonPropertyName "followers_count">]
        FollowersCount : int option
        [<System.Text.Json.Serialization.JsonPropertyName "following_count">]
        FollowingCount : int option
        [<System.Text.Json.Serialization.JsonPropertyName "full_name">]
        FullName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "is_admin">]
        IsAdmin : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "language">]
        Language : string option
        [<System.Text.Json.Serialization.JsonPropertyName "last_login">]
        LastLogin : string option
        [<System.Text.Json.Serialization.JsonPropertyName "location">]
        Location : string option
        [<System.Text.Json.Serialization.JsonPropertyName "login">]
        Login : string option
        [<System.Text.Json.Serialization.JsonPropertyName "login_name">]
        LoginName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "prohibit_login">]
        ProhibitLogin : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "restricted">]
        Restricted : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "starred_repos_count">]
        StarredReposCount : int option
        [<System.Text.Json.Serialization.JsonPropertyName "visibility">]
        Visibility : string option
        [<System.Text.Json.Serialization.JsonPropertyName "website">]
        Website : string option
    }

/// UserHeatmapData represents the data needed to create a heatmap
[<JsonParse true ; JsonSerialize true>]
type UserHeatmapData =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "contributions">]
        Contributions : int option
        [<System.Text.Json.Serialization.JsonPropertyName "timestamp">]
        Timestamp : int option
    }

/// UserSettings represents user settings
[<JsonParse true ; JsonSerialize true>]
type UserSettings =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "diff_view_style">]
        DiffViewStyle : string option
        [<System.Text.Json.Serialization.JsonPropertyName "full_name">]
        FullName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "hide_activity">]
        HideActivity : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "hide_email">]
        HideEmail : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "language">]
        Language : string option
        [<System.Text.Json.Serialization.JsonPropertyName "location">]
        Location : string option
        [<System.Text.Json.Serialization.JsonPropertyName "theme">]
        Theme : string option
        [<System.Text.Json.Serialization.JsonPropertyName "website">]
        Website : string option
    }

/// UserSettingsOptions represents options to change user settings
[<JsonParse true ; JsonSerialize true>]
type UserSettingsOptions =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "diff_view_style">]
        DiffViewStyle : string option
        [<System.Text.Json.Serialization.JsonPropertyName "full_name">]
        FullName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "hide_activity">]
        HideActivity : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "hide_email">]
        HideEmail : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "language">]
        Language : string option
        [<System.Text.Json.Serialization.JsonPropertyName "location">]
        Location : string option
        [<System.Text.Json.Serialization.JsonPropertyName "theme">]
        Theme : string option
        [<System.Text.Json.Serialization.JsonPropertyName "website">]
        Website : string option
    }

/// WatchInfo represents an API watch status of one repository
[<JsonParse true ; JsonSerialize true>]
type WatchInfo =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "ignored">]
        Ignored : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "reason">]
        Reason : unit option
        [<System.Text.Json.Serialization.JsonPropertyName "repository_url">]
        RepositoryUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "subscribed">]
        Subscribed : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// WikiCommit page commit/revision
[<JsonParse true ; JsonSerialize true>]
type WikiCommit =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "author">]
        Author : CommitUser option
        [<System.Text.Json.Serialization.JsonPropertyName "commiter">]
        Commiter : CommitUser option
        [<System.Text.Json.Serialization.JsonPropertyName "message">]
        Message : string option
        [<System.Text.Json.Serialization.JsonPropertyName "sha">]
        Sha : string option
    }

/// WikiCommitList commit/revision list
[<JsonParse true ; JsonSerialize true>]
type WikiCommitList =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "commits">]
        Commits : WikiCommit list option
        [<System.Text.Json.Serialization.JsonPropertyName "count">]
        Count : int option
    }

/// WikiPage a wiki page
[<JsonParse true ; JsonSerialize true>]
type WikiPage =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "commit_count">]
        CommitCount : int option
        [<System.Text.Json.Serialization.JsonPropertyName "content_base64">]
        ContentBase64 : string option
        [<System.Text.Json.Serialization.JsonPropertyName "footer">]
        Footer : string option
        [<System.Text.Json.Serialization.JsonPropertyName "html_url">]
        HtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "last_commit">]
        LastCommit : WikiCommit option
        [<System.Text.Json.Serialization.JsonPropertyName "sidebar">]
        Sidebar : string option
        [<System.Text.Json.Serialization.JsonPropertyName "sub_url">]
        SubUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string option
    }

/// WikiPageMetaData wiki page meta information
[<JsonParse true ; JsonSerialize true>]
type WikiPageMetaData =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "html_url">]
        HtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "last_commit">]
        LastCommit : WikiCommit option
        [<System.Text.Json.Serialization.JsonPropertyName "sub_url">]
        SubUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string option
    }

/// Comment represents a comment on a commit or issue
[<JsonParse true ; JsonSerialize true>]
type Comment =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "assets">]
        Assets : Attachment list option
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "html_url">]
        HtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "issue_url">]
        IssueUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "original_author">]
        OriginalAuthor : string option
        [<System.Text.Json.Serialization.JsonPropertyName "original_author_id">]
        OriginalAuthorId : int option
        [<System.Text.Json.Serialization.JsonPropertyName "pull_request_url">]
        PullRequestUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "updated_at">]
        UpdatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "user">]
        User : User option
    }

/// CommitStatus holds a single status of a single Commit
[<JsonParse true ; JsonSerialize true>]
type CommitStatus =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "context">]
        Context : string option
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "creator">]
        Creator : User option
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "status">]
        Status : string option
        [<System.Text.Json.Serialization.JsonPropertyName "target_url">]
        TargetUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "updated_at">]
        UpdatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// ContentsResponse contains information about a repo's entry's (dir, file, symlink, submodule) metadata and content
[<JsonParse true ; JsonSerialize true>]
type ContentsResponse =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "_links">]
        Links : FileLinksResponse option
        [<System.Text.Json.Serialization.JsonPropertyName "content">]
        Content : string option
        [<System.Text.Json.Serialization.JsonPropertyName "download_url">]
        DownloadUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "encoding">]
        Encoding : string option
        [<System.Text.Json.Serialization.JsonPropertyName "git_url">]
        GitUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "html_url">]
        HtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "last_commit_sha">]
        LastCommitSha : string option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "path">]
        Path : string option
        [<System.Text.Json.Serialization.JsonPropertyName "sha">]
        Sha : string option
        [<System.Text.Json.Serialization.JsonPropertyName "size">]
        Size : int option
        [<System.Text.Json.Serialization.JsonPropertyName "submodule_git_url">]
        SubmoduleGitUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "target">]
        Target : string option
        [<System.Text.Json.Serialization.JsonPropertyName "type">]
        Type : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// CreateFileOptions options for creating files
/// Note: `author` and `committer` are optional (if only one is given, it will be used for the other, otherwise the authenticated user will be used)
[<JsonParse true ; JsonSerialize true>]
type CreateFileOptions =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "author">]
        Author : Identity option
        [<System.Text.Json.Serialization.JsonPropertyName "branch">]
        Branch : string option
        [<System.Text.Json.Serialization.JsonPropertyName "committer">]
        Committer : Identity option
        [<System.Text.Json.Serialization.JsonPropertyName "content">]
        Content : string
        [<System.Text.Json.Serialization.JsonPropertyName "dates">]
        Dates : CommitDateOptions option
        [<System.Text.Json.Serialization.JsonPropertyName "message">]
        Message : string option
        [<System.Text.Json.Serialization.JsonPropertyName "new_branch">]
        NewBranch : string option
        [<System.Text.Json.Serialization.JsonPropertyName "signoff">]
        Signoff : bool option
    }

/// CreateHookOption options when create a hook
[<JsonParse true ; JsonSerialize true>]
type CreateHookOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "active">]
        Active : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "authorization_header">]
        AuthorizationHeader : string option
        [<System.Text.Json.Serialization.JsonPropertyName "branch_filter">]
        BranchFilter : string option
        [<System.Text.Json.Serialization.JsonPropertyName "config">]
        Config : CreateHookOptionConfig
        [<System.Text.Json.Serialization.JsonPropertyName "events">]
        Events : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "type">]
        Type : string
    }

/// CreatePullReviewOptions are options to create a pull review
[<JsonParse true ; JsonSerialize true>]
type CreatePullReviewOptions =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "comments">]
        Comments : CreatePullReviewComment list option
        [<System.Text.Json.Serialization.JsonPropertyName "commit_id">]
        CommitId : string option
        [<System.Text.Json.Serialization.JsonPropertyName "event">]
        Event : string option
    }

/// DeleteFileOptions options for deleting files (used for other File structs below)
/// Note: `author` and `committer` are optional (if only one is given, it will be used for the other, otherwise the authenticated user will be used)
[<JsonParse true ; JsonSerialize true>]
type DeleteFileOptions =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "author">]
        Author : Identity option
        [<System.Text.Json.Serialization.JsonPropertyName "branch">]
        Branch : string option
        [<System.Text.Json.Serialization.JsonPropertyName "committer">]
        Committer : Identity option
        [<System.Text.Json.Serialization.JsonPropertyName "dates">]
        Dates : CommitDateOptions option
        [<System.Text.Json.Serialization.JsonPropertyName "message">]
        Message : string option
        [<System.Text.Json.Serialization.JsonPropertyName "new_branch">]
        NewBranch : string option
        [<System.Text.Json.Serialization.JsonPropertyName "sha">]
        Sha : string
        [<System.Text.Json.Serialization.JsonPropertyName "signoff">]
        Signoff : bool option
    }

/// EditRepoOption options when editing a repository's properties
[<JsonParse true ; JsonSerialize true>]
type EditRepoOption =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "allow_manual_merge">]
        AllowManualMerge : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "allow_merge_commits">]
        AllowMergeCommits : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "allow_rebase">]
        AllowRebase : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "allow_rebase_explicit">]
        AllowRebaseExplicit : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "allow_rebase_update">]
        AllowRebaseUpdate : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "allow_squash_merge">]
        AllowSquashMerge : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "archived">]
        Archived : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "autodetect_manual_merge">]
        AutodetectManualMerge : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "default_allow_maintainer_edit">]
        DefaultAllowMaintainerEdit : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "default_branch">]
        DefaultBranch : string option
        [<System.Text.Json.Serialization.JsonPropertyName "default_delete_branch_after_merge">]
        DefaultDeleteBranchAfterMerge : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "default_merge_style">]
        DefaultMergeStyle : string option
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_prune">]
        EnablePrune : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "external_tracker">]
        ExternalTracker : ExternalTracker option
        [<System.Text.Json.Serialization.JsonPropertyName "external_wiki">]
        ExternalWiki : ExternalWiki option
        [<System.Text.Json.Serialization.JsonPropertyName "has_issues">]
        HasIssues : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "has_projects">]
        HasProjects : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "has_pull_requests">]
        HasPullRequests : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "has_wiki">]
        HasWiki : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "ignore_whitespace_conflicts">]
        IgnoreWhitespaceConflicts : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "internal_tracker">]
        InternalTracker : InternalTracker option
        [<System.Text.Json.Serialization.JsonPropertyName "mirror_interval">]
        MirrorInterval : string option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "private">]
        Private : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "template">]
        Template : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "website">]
        Website : string option
    }

/// IssueFormField represents a form field
[<JsonParse true ; JsonSerialize true>]
type IssueFormField =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "attributes">]
        Attributes : Type5 option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : string option
        [<System.Text.Json.Serialization.JsonPropertyName "type">]
        Type : string option
        [<System.Text.Json.Serialization.JsonPropertyName "validations">]
        Validations : Type6 option
    }

/// IssueTemplate represents an issue template for a repository
[<JsonParse true ; JsonSerialize true>]
type IssueTemplate =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "about">]
        About : string option
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : IssueFormField list option
        [<System.Text.Json.Serialization.JsonPropertyName "content">]
        Content : string option
        [<System.Text.Json.Serialization.JsonPropertyName "file_name">]
        FileName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "labels">]
        Labels : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "ref">]
        Ref : string option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string option
    }

/// Milestone milestone is a collection of issues on one repository
[<JsonParse true ; JsonSerialize true>]
type Milestone =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "closed_at">]
        ClosedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "closed_issues">]
        ClosedIssues : int option
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "due_on">]
        DueOn : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "open_issues">]
        OpenIssues : int option
        [<System.Text.Json.Serialization.JsonPropertyName "state">]
        State : string option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string option
        [<System.Text.Json.Serialization.JsonPropertyName "updated_at">]
        UpdatedAt : string option
    }

/// NodeInfoUsage contains usage statistics for this server
[<JsonParse true ; JsonSerialize true>]
type NodeInfoUsage =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "localComments">]
        LocalComments : int option
        [<System.Text.Json.Serialization.JsonPropertyName "localPosts">]
        LocalPosts : int option
        [<System.Text.Json.Serialization.JsonPropertyName "users">]
        Users : NodeInfoUsageUsers option
    }

/// NotificationSubject contains the notification subject (Issue/Pull/Commit)
[<JsonParse true ; JsonSerialize true>]
type NotificationSubject =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "html_url">]
        HtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "latest_comment_html_url">]
        LatestCommentHtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "latest_comment_url">]
        LatestCommentUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "state">]
        State : string option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string option
        [<System.Text.Json.Serialization.JsonPropertyName "type">]
        Type : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// PayloadCommitVerification represents the GPG verification of a commit
[<JsonParse true ; JsonSerialize true>]
type PayloadCommitVerification =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "payload">]
        Payload : string option
        [<System.Text.Json.Serialization.JsonPropertyName "reason">]
        Reason : string option
        [<System.Text.Json.Serialization.JsonPropertyName "signature">]
        Signature : string option
        [<System.Text.Json.Serialization.JsonPropertyName "signer">]
        Signer : PayloadUser option
        [<System.Text.Json.Serialization.JsonPropertyName "verified">]
        Verified : bool option
    }

/// PublicKey publickey is a user key to push code to repository
[<JsonParse true ; JsonSerialize true>]
type PublicKey =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "fingerprint">]
        Fingerprint : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "key">]
        Key : string option
        [<System.Text.Json.Serialization.JsonPropertyName "key_type">]
        KeyType : string option
        [<System.Text.Json.Serialization.JsonPropertyName "read_only">]
        ReadOnly : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
        [<System.Text.Json.Serialization.JsonPropertyName "user">]
        User : User option
    }

/// PullReview represents a pull request review
[<JsonParse true ; JsonSerialize true>]
type PullReview =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "comments_count">]
        CommentsCount : int option
        [<System.Text.Json.Serialization.JsonPropertyName "commit_id">]
        CommitId : string option
        [<System.Text.Json.Serialization.JsonPropertyName "dismissed">]
        Dismissed : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "html_url">]
        HtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "official">]
        Official : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "pull_request_url">]
        PullRequestUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "stale">]
        Stale : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "state">]
        State : string option
        [<System.Text.Json.Serialization.JsonPropertyName "submitted_at">]
        SubmittedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "team">]
        Team : Team option
        [<System.Text.Json.Serialization.JsonPropertyName "updated_at">]
        UpdatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "user">]
        User : User option
    }

/// PullReviewComment represents a comment on a pull request review
[<JsonParse true ; JsonSerialize true>]
type PullReviewComment =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "commit_id">]
        CommitId : string option
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "diff_hunk">]
        DiffHunk : string option
        [<System.Text.Json.Serialization.JsonPropertyName "html_url">]
        HtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "original_commit_id">]
        OriginalCommitId : string option
        [<System.Text.Json.Serialization.JsonPropertyName "original_position">]
        OriginalPosition : int option
        [<System.Text.Json.Serialization.JsonPropertyName "path">]
        Path : string option
        [<System.Text.Json.Serialization.JsonPropertyName "position">]
        Position : int option
        [<System.Text.Json.Serialization.JsonPropertyName "pull_request_review_id">]
        PullRequestReviewId : int option
        [<System.Text.Json.Serialization.JsonPropertyName "pull_request_url">]
        PullRequestUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "resolver">]
        Resolver : User option
        [<System.Text.Json.Serialization.JsonPropertyName "updated_at">]
        UpdatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "user">]
        User : User option
    }

/// Reaction contain one reaction
[<JsonParse true ; JsonSerialize true>]
type Reaction =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "content">]
        Content : string option
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "user">]
        User : User option
    }

/// Release represents a repository release
[<JsonParse true ; JsonSerialize true>]
type Release =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "assets">]
        Assets : Attachment list option
        [<System.Text.Json.Serialization.JsonPropertyName "author">]
        Author : User option
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "draft">]
        Draft : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "html_url">]
        HtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "prerelease">]
        Prerelease : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "published_at">]
        PublishedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "tag_name">]
        TagName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "tarball_url">]
        TarballUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "target_commitish">]
        TargetCommitish : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
        [<System.Text.Json.Serialization.JsonPropertyName "zipball_url">]
        ZipballUrl : string option
    }

/// RepoCollaboratorPermission to get repository permission for a collaborator
[<JsonParse true ; JsonSerialize true>]
type RepoCollaboratorPermission =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "permission">]
        Permission : string option
        [<System.Text.Json.Serialization.JsonPropertyName "role_name">]
        RoleName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "user">]
        User : User option
    }

/// RepoCommit contains information of a commit in the context of a repository.
[<JsonParse true ; JsonSerialize true>]
type RepoCommit =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "author">]
        Author : CommitUser option
        [<System.Text.Json.Serialization.JsonPropertyName "committer">]
        Committer : CommitUser option
        [<System.Text.Json.Serialization.JsonPropertyName "message">]
        Message : string option
        [<System.Text.Json.Serialization.JsonPropertyName "tree">]
        Tree : CommitMeta option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
        [<System.Text.Json.Serialization.JsonPropertyName "verification">]
        Verification : PayloadCommitVerification option
    }

/// RepoTransfer represents a pending repo transfer
[<JsonParse true ; JsonSerialize true>]
type RepoTransfer =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "doer">]
        Doer : User option
        [<System.Text.Json.Serialization.JsonPropertyName "recipient">]
        Recipient : User option
        [<System.Text.Json.Serialization.JsonPropertyName "teams">]
        Teams : Team list option
    }

/// Repository represents a repository
[<JsonParse true ; JsonSerialize true>]
type Repository =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "allow_merge_commits">]
        AllowMergeCommits : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "allow_rebase">]
        AllowRebase : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "allow_rebase_explicit">]
        AllowRebaseExplicit : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "allow_rebase_update">]
        AllowRebaseUpdate : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "allow_squash_merge">]
        AllowSquashMerge : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "archived">]
        Archived : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "avatar_url">]
        AvatarUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "clone_url">]
        CloneUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "default_allow_maintainer_edit">]
        DefaultAllowMaintainerEdit : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "default_branch">]
        DefaultBranch : string option
        [<System.Text.Json.Serialization.JsonPropertyName "default_delete_branch_after_merge">]
        DefaultDeleteBranchAfterMerge : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "default_merge_style">]
        DefaultMergeStyle : string option
        [<System.Text.Json.Serialization.JsonPropertyName "description">]
        Description : string option
        [<System.Text.Json.Serialization.JsonPropertyName "empty">]
        Empty : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "external_tracker">]
        ExternalTracker : ExternalTracker option
        [<System.Text.Json.Serialization.JsonPropertyName "external_wiki">]
        ExternalWiki : ExternalWiki option
        [<System.Text.Json.Serialization.JsonPropertyName "fork">]
        Fork : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "forks_count">]
        ForksCount : int option
        [<System.Text.Json.Serialization.JsonPropertyName "full_name">]
        FullName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "has_issues">]
        HasIssues : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "has_projects">]
        HasProjects : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "has_pull_requests">]
        HasPullRequests : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "has_wiki">]
        HasWiki : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "html_url">]
        HtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "ignore_whitespace_conflicts">]
        IgnoreWhitespaceConflicts : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "internal">]
        Internal : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "internal_tracker">]
        InternalTracker : InternalTracker option
        [<System.Text.Json.Serialization.JsonPropertyName "language">]
        Language : string option
        [<System.Text.Json.Serialization.JsonPropertyName "languages_url">]
        LanguagesUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "link">]
        Link : string option
        [<System.Text.Json.Serialization.JsonPropertyName "mirror">]
        Mirror : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "mirror_interval">]
        MirrorInterval : string option
        [<System.Text.Json.Serialization.JsonPropertyName "mirror_updated">]
        MirrorUpdated : string option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "open_issues_count">]
        OpenIssuesCount : int option
        [<System.Text.Json.Serialization.JsonPropertyName "open_pr_counter">]
        OpenPrCounter : int option
        [<System.Text.Json.Serialization.JsonPropertyName "original_url">]
        OriginalUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "owner">]
        Owner : User option
        [<System.Text.Json.Serialization.JsonPropertyName "parent">]
        Parent : Repository option
        [<System.Text.Json.Serialization.JsonPropertyName "permissions">]
        Permissions : Permission option
        [<System.Text.Json.Serialization.JsonPropertyName "private">]
        Private : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "release_counter">]
        ReleaseCounter : int option
        [<System.Text.Json.Serialization.JsonPropertyName "repo_transfer">]
        RepoTransfer : RepoTransfer option
        [<System.Text.Json.Serialization.JsonPropertyName "size">]
        Size : int option
        [<System.Text.Json.Serialization.JsonPropertyName "ssh_url">]
        SshUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "stars_count">]
        StarsCount : int option
        [<System.Text.Json.Serialization.JsonPropertyName "template">]
        Template : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "updated_at">]
        UpdatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "watchers_count">]
        WatchersCount : int option
        [<System.Text.Json.Serialization.JsonPropertyName "website">]
        Website : string option
    }

/// SearchResults results of a successful search
[<JsonParse true ; JsonSerialize true>]
type SearchResults =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "data">]
        Data : Repository list option
        [<System.Text.Json.Serialization.JsonPropertyName "ok">]
        Ok : bool option
    }

/// AnnotatedTag represents an annotated tag
[<JsonParse true ; JsonSerialize true>]
type AnnotatedTag =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "message">]
        Message : string option
        [<System.Text.Json.Serialization.JsonPropertyName "object">]
        Object : AnnotatedTagObject option
        [<System.Text.Json.Serialization.JsonPropertyName "sha">]
        Sha : string option
        [<System.Text.Json.Serialization.JsonPropertyName "tag">]
        Tag : string option
        [<System.Text.Json.Serialization.JsonPropertyName "tagger">]
        Tagger : CommitUser option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
        [<System.Text.Json.Serialization.JsonPropertyName "verification">]
        Verification : PayloadCommitVerification option
    }

/// CombinedStatus holds the combined state of several statuses for a single commit
[<JsonParse true ; JsonSerialize true>]
type CombinedStatus =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "commit_url">]
        CommitUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "repository">]
        Repository : Repository option
        [<System.Text.Json.Serialization.JsonPropertyName "sha">]
        Sha : string option
        [<System.Text.Json.Serialization.JsonPropertyName "state">]
        State : string option
        [<System.Text.Json.Serialization.JsonPropertyName "statuses">]
        Statuses : CommitStatus list option
        [<System.Text.Json.Serialization.JsonPropertyName "total_count">]
        TotalCount : int option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// Commit contains information generated from a Git commit.
[<JsonParse true ; JsonSerialize true>]
type Commit =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "author">]
        Author : User option
        [<System.Text.Json.Serialization.JsonPropertyName "commit">]
        Commit : RepoCommit option
        [<System.Text.Json.Serialization.JsonPropertyName "committer">]
        Committer : User option
        [<System.Text.Json.Serialization.JsonPropertyName "created">]
        Created : string option
        [<System.Text.Json.Serialization.JsonPropertyName "files">]
        Files : CommitAffectedFiles list option
        [<System.Text.Json.Serialization.JsonPropertyName "html_url">]
        HtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "parents">]
        Parents : CommitMeta list option
        [<System.Text.Json.Serialization.JsonPropertyName "sha">]
        Sha : string option
        [<System.Text.Json.Serialization.JsonPropertyName "stats">]
        Stats : CommitStats option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// DeployKey a deploy key
[<JsonParse true ; JsonSerialize true>]
type DeployKey =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "fingerprint">]
        Fingerprint : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "key">]
        Key : string option
        [<System.Text.Json.Serialization.JsonPropertyName "key_id">]
        KeyId : int option
        [<System.Text.Json.Serialization.JsonPropertyName "read_only">]
        ReadOnly : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "repository">]
        Repository : Repository option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// FileDeleteResponse contains information about a repo's file that was deleted
[<JsonParse true ; JsonSerialize true>]
type FileDeleteResponse =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "commit">]
        Commit : FileCommitResponse option
        [<System.Text.Json.Serialization.JsonPropertyName "content">]
        Content : unit option
        [<System.Text.Json.Serialization.JsonPropertyName "verification">]
        Verification : PayloadCommitVerification option
    }

/// FileResponse contains information about a repo's file
[<JsonParse true ; JsonSerialize true>]
type FileResponse =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "commit">]
        Commit : FileCommitResponse option
        [<System.Text.Json.Serialization.JsonPropertyName "content">]
        Content : ContentsResponse option
        [<System.Text.Json.Serialization.JsonPropertyName "verification">]
        Verification : PayloadCommitVerification option
    }

/// Issue represents an issue in a repository
[<JsonParse true ; JsonSerialize true>]
type Issue =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "assets">]
        Assets : Attachment list option
        [<System.Text.Json.Serialization.JsonPropertyName "assignee">]
        Assignee : User option
        [<System.Text.Json.Serialization.JsonPropertyName "assignees">]
        Assignees : User list option
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "closed_at">]
        ClosedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "comments">]
        Comments : int option
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "due_date">]
        DueDate : string option
        [<System.Text.Json.Serialization.JsonPropertyName "html_url">]
        HtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "is_locked">]
        IsLocked : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "labels">]
        Labels : Label list option
        [<System.Text.Json.Serialization.JsonPropertyName "milestone">]
        Milestone : Milestone option
        [<System.Text.Json.Serialization.JsonPropertyName "number">]
        Number : int option
        [<System.Text.Json.Serialization.JsonPropertyName "original_author">]
        OriginalAuthor : string option
        [<System.Text.Json.Serialization.JsonPropertyName "original_author_id">]
        OriginalAuthorId : int option
        [<System.Text.Json.Serialization.JsonPropertyName "pull_request">]
        PullRequest : PullRequestMeta option
        [<System.Text.Json.Serialization.JsonPropertyName "ref">]
        Ref : string option
        [<System.Text.Json.Serialization.JsonPropertyName "repository">]
        Repository : RepositoryMeta option
        [<System.Text.Json.Serialization.JsonPropertyName "state">]
        State : string option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string option
        [<System.Text.Json.Serialization.JsonPropertyName "updated_at">]
        UpdatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
        [<System.Text.Json.Serialization.JsonPropertyName "user">]
        User : User option
    }

/// NodeInfo contains standardized way of exposing metadata about a server running one of the distributed social networks
[<JsonParse true ; JsonSerialize true>]
type NodeInfo =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "metadata">]
        Metadata : Type7 option
        [<System.Text.Json.Serialization.JsonPropertyName "openRegistrations">]
        OpenRegistrations : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "protocols">]
        Protocols : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "services">]
        Services : NodeInfoServices option
        [<System.Text.Json.Serialization.JsonPropertyName "software">]
        Software : NodeInfoSoftware option
        [<System.Text.Json.Serialization.JsonPropertyName "usage">]
        Usage : NodeInfoUsage option
        [<System.Text.Json.Serialization.JsonPropertyName "version">]
        Version : string option
    }

/// Note contains information related to a git note
[<JsonParse true ; JsonSerialize true>]
type Note =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "commit">]
        Commit : Commit option
        [<System.Text.Json.Serialization.JsonPropertyName "message">]
        Message : string option
    }

/// NotificationThread expose Notification on API
[<JsonParse true ; JsonSerialize true>]
type NotificationThread =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "pinned">]
        Pinned : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "repository">]
        Repository : Repository option
        [<System.Text.Json.Serialization.JsonPropertyName "subject">]
        Subject : NotificationSubject option
        [<System.Text.Json.Serialization.JsonPropertyName "unread">]
        Unread : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "updated_at">]
        UpdatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
    }

/// PRBranchInfo information about a branch
[<JsonParse true ; JsonSerialize true>]
type PRBranchInfo =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "label">]
        Label : string option
        [<System.Text.Json.Serialization.JsonPropertyName "ref">]
        Ref : string option
        [<System.Text.Json.Serialization.JsonPropertyName "repo">]
        Repo : Repository option
        [<System.Text.Json.Serialization.JsonPropertyName "repo_id">]
        RepoId : int option
        [<System.Text.Json.Serialization.JsonPropertyName "sha">]
        Sha : string option
    }

/// Package represents a package
[<JsonParse true ; JsonSerialize true>]
type Package =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "creator">]
        Creator : User option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "owner">]
        Owner : User option
        [<System.Text.Json.Serialization.JsonPropertyName "repository">]
        Repository : Repository option
        [<System.Text.Json.Serialization.JsonPropertyName "type">]
        Type : string option
        [<System.Text.Json.Serialization.JsonPropertyName "version">]
        Version : string option
    }

/// PayloadCommit represents a commit
[<JsonParse true ; JsonSerialize true>]
type PayloadCommit =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "added">]
        Added : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "author">]
        Author : PayloadUser option
        [<System.Text.Json.Serialization.JsonPropertyName "committer">]
        Committer : PayloadUser option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : string option
        [<System.Text.Json.Serialization.JsonPropertyName "message">]
        Message : string option
        [<System.Text.Json.Serialization.JsonPropertyName "modified">]
        Modified : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "removed">]
        Removed : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "timestamp">]
        Timestamp : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
        [<System.Text.Json.Serialization.JsonPropertyName "verification">]
        Verification : PayloadCommitVerification option
    }

/// PullRequest represents a pull request
[<JsonParse true ; JsonSerialize true>]
type PullRequest =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "allow_maintainer_edit">]
        AllowMaintainerEdit : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "assignee">]
        Assignee : User option
        [<System.Text.Json.Serialization.JsonPropertyName "assignees">]
        Assignees : User list option
        [<System.Text.Json.Serialization.JsonPropertyName "base">]
        Base : PRBranchInfo option
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "closed_at">]
        ClosedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "comments">]
        Comments : int option
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "diff_url">]
        DiffUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "due_date">]
        DueDate : string option
        [<System.Text.Json.Serialization.JsonPropertyName "head">]
        Head : PRBranchInfo option
        [<System.Text.Json.Serialization.JsonPropertyName "html_url">]
        HtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "is_locked">]
        IsLocked : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "labels">]
        Labels : Label list option
        [<System.Text.Json.Serialization.JsonPropertyName "merge_base">]
        MergeBase : string option
        [<System.Text.Json.Serialization.JsonPropertyName "merge_commit_sha">]
        MergeCommitSha : string option
        [<System.Text.Json.Serialization.JsonPropertyName "mergeable">]
        Mergeable : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "merged">]
        Merged : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "merged_at">]
        MergedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "merged_by">]
        MergedBy : User option
        [<System.Text.Json.Serialization.JsonPropertyName "milestone">]
        Milestone : Milestone option
        [<System.Text.Json.Serialization.JsonPropertyName "number">]
        Number : int option
        [<System.Text.Json.Serialization.JsonPropertyName "patch_url">]
        PatchUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "state">]
        State : string option
        [<System.Text.Json.Serialization.JsonPropertyName "title">]
        Title : string option
        [<System.Text.Json.Serialization.JsonPropertyName "updated_at">]
        UpdatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "url">]
        Url : string option
        [<System.Text.Json.Serialization.JsonPropertyName "user">]
        User : User option
    }

/// TrackedTime worked time for an issue / pr
[<JsonParse true ; JsonSerialize true>]
type TrackedTime =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "created">]
        Created : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "issue">]
        Issue : Issue option
        [<System.Text.Json.Serialization.JsonPropertyName "issue_id">]
        IssueId : int option
        [<System.Text.Json.Serialization.JsonPropertyName "time">]
        Time : int option
        [<System.Text.Json.Serialization.JsonPropertyName "user_id">]
        UserId : int option
        [<System.Text.Json.Serialization.JsonPropertyName "user_name">]
        UserName : string option
    }

/// Branch represents a repository branch
[<JsonParse true ; JsonSerialize true>]
type Branch =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "commit">]
        Commit : PayloadCommit option
        [<System.Text.Json.Serialization.JsonPropertyName "effective_branch_protection_name">]
        EffectiveBranchProtectionName : string option
        [<System.Text.Json.Serialization.JsonPropertyName "enable_status_check">]
        EnableStatusCheck : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "name">]
        Name : string option
        [<System.Text.Json.Serialization.JsonPropertyName "protected">]
        Protected : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "required_approvals">]
        RequiredApprovals : int option
        [<System.Text.Json.Serialization.JsonPropertyName "status_check_contexts">]
        StatusCheckContexts : string list option
        [<System.Text.Json.Serialization.JsonPropertyName "user_can_merge">]
        UserCanMerge : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "user_can_push">]
        UserCanPush : bool option
    }

/// TimelineComment represents a timeline comment (comment of any type) on a commit or issue
[<JsonParse true ; JsonSerialize true>]
type TimelineComment =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        [<System.Text.Json.Serialization.JsonPropertyName "assignee">]
        Assignee : User option
        [<System.Text.Json.Serialization.JsonPropertyName "assignee_team">]
        AssigneeTeam : Team option
        [<System.Text.Json.Serialization.JsonPropertyName "body">]
        Body : string option
        [<System.Text.Json.Serialization.JsonPropertyName "created_at">]
        CreatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "dependent_issue">]
        DependentIssue : Issue option
        [<System.Text.Json.Serialization.JsonPropertyName "html_url">]
        HtmlUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "id">]
        Id : int option
        [<System.Text.Json.Serialization.JsonPropertyName "issue_url">]
        IssueUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "label">]
        Label : Label option
        [<System.Text.Json.Serialization.JsonPropertyName "milestone">]
        Milestone : Milestone option
        [<System.Text.Json.Serialization.JsonPropertyName "new_ref">]
        NewRef : string option
        [<System.Text.Json.Serialization.JsonPropertyName "new_title">]
        NewTitle : string option
        [<System.Text.Json.Serialization.JsonPropertyName "old_milestone">]
        OldMilestone : Milestone option
        [<System.Text.Json.Serialization.JsonPropertyName "old_project_id">]
        OldProjectId : int option
        [<System.Text.Json.Serialization.JsonPropertyName "old_ref">]
        OldRef : string option
        [<System.Text.Json.Serialization.JsonPropertyName "old_title">]
        OldTitle : string option
        [<System.Text.Json.Serialization.JsonPropertyName "project_id">]
        ProjectId : int option
        [<System.Text.Json.Serialization.JsonPropertyName "pull_request_url">]
        PullRequestUrl : string option
        [<System.Text.Json.Serialization.JsonPropertyName "ref_action">]
        RefAction : string option
        [<System.Text.Json.Serialization.JsonPropertyName "ref_comment">]
        RefComment : Comment option
        [<System.Text.Json.Serialization.JsonPropertyName "ref_commit_sha">]
        RefCommitSha : string option
        [<System.Text.Json.Serialization.JsonPropertyName "ref_issue">]
        RefIssue : Issue option
        [<System.Text.Json.Serialization.JsonPropertyName "removed_assignee">]
        RemovedAssignee : bool option
        [<System.Text.Json.Serialization.JsonPropertyName "resolve_doer">]
        ResolveDoer : User option
        [<System.Text.Json.Serialization.JsonPropertyName "review_id">]
        ReviewId : int option
        [<System.Text.Json.Serialization.JsonPropertyName "tracked_time">]
        TrackedTime : TrackedTime option
        [<System.Text.Json.Serialization.JsonPropertyName "type">]
        Type : string option
        [<System.Text.Json.Serialization.JsonPropertyName "updated_at">]
        UpdatedAt : string option
        [<System.Text.Json.Serialization.JsonPropertyName "user">]
        User : User option
    }

[<JsonParse true ; JsonSerialize true>]
type LanguageStatistics =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, int>
    }

/// This documentation describes the Gitea API.
[<HttpClient false ; RestEase.BasePath "/api/v1">]
type IGitea =
    /// Returns the Person actor for a user
    [<RestEase.Get "activitypub/user/{username}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract ActivitypubPerson :
        [<RestEase.Path "username">] username : string * ?ct : System.Threading.CancellationToken ->
            ActivityPub System.Threading.Tasks.Task

    /// Send to the inbox
    [<RestEase.Post "activitypub/user/{username}/inbox">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract ActivitypubPersonInbox :
        [<RestEase.Path "username">] username : string * ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List cron tasks
    [<RestEase.Get "admin/cron">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminCronList :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Cron list System.Threading.Tasks.Task

    /// Run cron task
    [<RestEase.Post "admin/cron/{task}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminCronRun :
        [<RestEase.Path "task">] task : string * ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List system's webhooks
    [<RestEase.Get "admin/hooks">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminListHooks :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Hook list System.Threading.Tasks.Task

    /// Create a hook
    [<RestEase.Post "admin/hooks">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminCreateHook :
        [<RestEase.Body>] body : CreateHookOption * ?ct : System.Threading.CancellationToken ->
            Hook System.Threading.Tasks.Task

    /// Get a hook
    [<RestEase.Get "admin/hooks/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminGetHook :
        [<RestEase.Path "id">] id : int * ?ct : System.Threading.CancellationToken -> Hook System.Threading.Tasks.Task

    /// Update a hook
    [<RestEase.Post "admin/hooks/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminEditHook :
        [<RestEase.Path "id">] id : int *
        [<RestEase.Body>] body : EditHookOption *
        ?ct : System.Threading.CancellationToken ->
            Hook System.Threading.Tasks.Task

    /// List all organizations
    [<RestEase.Get "admin/orgs">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminGetAllOrgs :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Organization list System.Threading.Tasks.Task

    /// List unadopted repositories
    [<RestEase.Get "admin/unadopted">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminUnadoptedList :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        [<RestEase.Query "pattern">] pattern : string *
        ?ct : System.Threading.CancellationToken ->
            string list System.Threading.Tasks.Task

    /// Adopt unadopted files as a repository
    [<RestEase.Post "admin/unadopted/{owner}/{repo}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminAdoptRepository :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Delete unadopted files
    [<RestEase.Delete "admin/unadopted/{owner}/{repo}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminDeleteUnadoptedRepository :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List all users
    [<RestEase.Get "admin/users">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminGetAllUsers :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            User list System.Threading.Tasks.Task

    /// Create a user
    [<RestEase.Post "admin/users">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminCreateUser :
        [<RestEase.Body>] body : CreateUserOption * ?ct : System.Threading.CancellationToken ->
            User System.Threading.Tasks.Task

    /// Delete a user
    [<RestEase.Delete "admin/users/{username}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminDeleteUser :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Query "purge">] purge : bool *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Edit an existing user
    [<RestEase.Post "admin/users/{username}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminEditUser :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Body>] body : EditUserOption *
        ?ct : System.Threading.CancellationToken ->
            User System.Threading.Tasks.Task

    /// Add a public key on behalf of a user
    [<RestEase.Post "admin/users/{username}/keys">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminCreatePublicKey :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Body>] key : CreateKeyOption *
        ?ct : System.Threading.CancellationToken ->
            PublicKey System.Threading.Tasks.Task

    /// Delete a user's public key
    [<RestEase.Delete "admin/users/{username}/keys/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminDeleteUserPublicKey :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Create an organization
    [<RestEase.Post "admin/users/{username}/orgs">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminCreateOrg :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Body>] organization : CreateOrgOption *
        ?ct : System.Threading.CancellationToken ->
            Organization System.Threading.Tasks.Task

    /// Create a repository on behalf of a user
    [<RestEase.Post "admin/users/{username}/repos">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminCreateRepo :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Body>] repository : CreateRepoOption *
        ?ct : System.Threading.CancellationToken ->
            Repository System.Threading.Tasks.Task

    /// Delete a hook
    [<RestEase.Delete "amdin/hooks/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AdminDeleteHook :
        [<RestEase.Path "id">] id : int * ?ct : System.Threading.CancellationToken -> unit System.Threading.Tasks.Task

    /// Render a markdown document as HTML
    [<RestEase.Post "markdown">]
    [<RestEase.Header("Content-Type", "html")>]
    abstract RenderMarkdown :
        [<RestEase.Body>] body : MarkdownOption * ?ct : System.Threading.CancellationToken ->
            string System.Threading.Tasks.Task

    /// Render raw markdown as HTML
    [<RestEase.Post "markdown/raw">]
    [<RestEase.Header("Content-Type", "html")>]
    abstract RenderMarkdownRaw :
        [<RestEase.Body>] body : string * ?ct : System.Threading.CancellationToken -> string System.Threading.Tasks.Task

    /// Returns the nodeinfo of the Gitea application
    [<RestEase.Get "nodeinfo">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract GetNodeInfo : ?ct : System.Threading.CancellationToken -> NodeInfo System.Threading.Tasks.Task

    /// List users's notification threads
    [<RestEase.Get "notifications">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract NotifyGetList :
        [<RestEase.Query "all">] all : bool *
        [<RestEase.Query "status-types">] status_types : string list *
        [<RestEase.Query "subject-type">] subject_type : string list *
        [<RestEase.Query "since">] since : string *
        [<RestEase.Query "before">] before : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            NotificationThread list System.Threading.Tasks.Task

    /// Mark notification threads as read, pinned or unread
    [<RestEase.Put "notifications">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract NotifyReadList :
        [<RestEase.Query "last_read_at">] last_read_at : string *
        [<RestEase.Query "all">] all : string *
        [<RestEase.Query "status-types">] status_types : string list *
        [<RestEase.Query "to-status">] to_status : string *
        ?ct : System.Threading.CancellationToken ->
            NotificationThread list System.Threading.Tasks.Task

    /// Check if unread notifications exist
    [<RestEase.Get "notifications/new">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract NotifyNewAvailable :
        ?ct : System.Threading.CancellationToken -> NotificationCount System.Threading.Tasks.Task

    /// Get notification thread by ID
    [<RestEase.Get "notifications/threads/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract NotifyGetThread :
        [<RestEase.Path "id">] id : string * ?ct : System.Threading.CancellationToken ->
            NotificationThread System.Threading.Tasks.Task

    /// Mark notification thread as read by ID
    [<RestEase.Post "notifications/threads/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract NotifyReadThread :
        [<RestEase.Path "id">] id : string *
        [<RestEase.Query "to-status">] to_status : string *
        ?ct : System.Threading.CancellationToken ->
            NotificationThread System.Threading.Tasks.Task

    /// Create a repository in an organization
    [<RestEase.Post "org/{org}/repos">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract CreateOrgRepoDeprecated :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Body>] body : CreateRepoOption *
        ?ct : System.Threading.CancellationToken ->
            Repository System.Threading.Tasks.Task

    /// Get list of organizations
    [<RestEase.Get "orgs">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgGetAll :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Organization list System.Threading.Tasks.Task

    /// Create an organization
    [<RestEase.Post "orgs">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgCreate :
        [<RestEase.Body>] organization : CreateOrgOption * ?ct : System.Threading.CancellationToken ->
            Organization System.Threading.Tasks.Task

    /// Get an organization
    [<RestEase.Get "orgs/{org}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgGet :
        [<RestEase.Path "org">] org : string * ?ct : System.Threading.CancellationToken ->
            Organization System.Threading.Tasks.Task

    /// Delete an organization
    [<RestEase.Delete "orgs/{org}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgDelete :
        [<RestEase.Path "org">] org : string * ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Edit an organization
    [<RestEase.Post "orgs/{org}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgEdit :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Body>] body : EditOrgOption *
        ?ct : System.Threading.CancellationToken ->
            Organization System.Threading.Tasks.Task

    /// List an organization's webhooks
    [<RestEase.Get "orgs/{org}/hooks">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgListHooks :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Hook list System.Threading.Tasks.Task

    /// Create a hook
    [<RestEase.Post "orgs/{org}/hooks">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgCreateHook :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Body>] body : CreateHookOption *
        ?ct : System.Threading.CancellationToken ->
            Hook System.Threading.Tasks.Task

    /// Get a hook
    [<RestEase.Get "orgs/{org}/hooks/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgGetHook :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            Hook System.Threading.Tasks.Task

    /// Delete a hook
    [<RestEase.Delete "orgs/{org}/hooks/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgDeleteHook :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Update a hook
    [<RestEase.Post "orgs/{org}/hooks/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgEditHook :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Path "id">] id : int *
        [<RestEase.Body>] body : EditHookOption *
        ?ct : System.Threading.CancellationToken ->
            Hook System.Threading.Tasks.Task

    /// List an organization's labels
    [<RestEase.Get "orgs/{org}/labels">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgListLabels :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Label list System.Threading.Tasks.Task

    /// Create a label for an organization
    [<RestEase.Post "orgs/{org}/labels">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgCreateLabel :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Body>] body : CreateLabelOption *
        ?ct : System.Threading.CancellationToken ->
            Label System.Threading.Tasks.Task

    /// Get a single label
    [<RestEase.Get "orgs/{org}/labels/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgGetLabel :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            Label System.Threading.Tasks.Task

    /// Delete a label
    [<RestEase.Delete "orgs/{org}/labels/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgDeleteLabel :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Update a label
    [<RestEase.Post "orgs/{org}/labels/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgEditLabel :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Path "id">] id : int *
        [<RestEase.Body>] body : EditLabelOption *
        ?ct : System.Threading.CancellationToken ->
            Label System.Threading.Tasks.Task

    /// List an organization's members
    [<RestEase.Get "orgs/{org}/members">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgListMembers :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            User list System.Threading.Tasks.Task

    /// Check if a user is a member of an organization
    [<RestEase.Get "orgs/{org}/members/{username}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgIsMember :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Path "username">] username : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Remove a member from an organization
    [<RestEase.Delete "orgs/{org}/members/{username}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgDeleteMember :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Path "username">] username : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List an organization's public members
    [<RestEase.Get "orgs/{org}/public_members">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgListPublicMembers :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            User list System.Threading.Tasks.Task

    /// Check if a user is a public member of an organization
    [<RestEase.Get "orgs/{org}/public_members/{username}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgIsPublicMember :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Path "username">] username : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Conceal a user's membership
    [<RestEase.Delete "orgs/{org}/public_members/{username}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgConcealMember :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Path "username">] username : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Publicize a user's membership
    [<RestEase.Put "orgs/{org}/public_members/{username}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgPublicizeMember :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Path "username">] username : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List an organization's repos
    [<RestEase.Get "orgs/{org}/repos">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgListRepos :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Repository list System.Threading.Tasks.Task

    /// Create a repository in an organization
    [<RestEase.Post "orgs/{org}/repos">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract CreateOrgRepo :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Body>] body : CreateRepoOption *
        ?ct : System.Threading.CancellationToken ->
            Repository System.Threading.Tasks.Task

    /// List an organization's teams
    [<RestEase.Get "orgs/{org}/teams">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgListTeams :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Team list System.Threading.Tasks.Task

    /// Create a team
    [<RestEase.Post "orgs/{org}/teams">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgCreateTeam :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Body>] body : CreateTeamOption *
        ?ct : System.Threading.CancellationToken ->
            Team System.Threading.Tasks.Task

    /// Search for teams within an organization
    [<RestEase.Get "orgs/{org}/teams/search">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract TeamSearch :
        [<RestEase.Path "org">] org : string *
        [<RestEase.Query "q">] q : string *
        [<RestEase.Query "include_desc">] include_desc : bool *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Gets all packages of an owner
    [<RestEase.Get "packages/{owner}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract ListPackages :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        [<RestEase.Query "type">] type' : string *
        [<RestEase.Query "q">] q : string *
        ?ct : System.Threading.CancellationToken ->
            Package list System.Threading.Tasks.Task

    /// Gets a package
    [<RestEase.Get "packages/{owner}/{type}/{name}/{version}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract GetPackage :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "type">] type' : string *
        [<RestEase.Path "name">] name : string *
        [<RestEase.Path "version">] version : string *
        ?ct : System.Threading.CancellationToken ->
            Package System.Threading.Tasks.Task

    /// Delete a package
    [<RestEase.Delete "packages/{owner}/{type}/{name}/{version}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract DeletePackage :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "type">] type' : string *
        [<RestEase.Path "name">] name : string *
        [<RestEase.Path "version">] version : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Gets all files of a package
    [<RestEase.Get "packages/{owner}/{type}/{name}/{version}/files">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract ListPackageFiles :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "type">] type' : string *
        [<RestEase.Path "name">] name : string *
        [<RestEase.Path "version">] version : string *
        ?ct : System.Threading.CancellationToken ->
            PackageFile list System.Threading.Tasks.Task

    /// Search for issues across the repositories that the user has access to
    [<RestEase.Get "repos/issues/search">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueSearchIssues :
        [<RestEase.Query "state">] state : string *
        [<RestEase.Query "labels">] labels : string *
        [<RestEase.Query "milestones">] milestones : string *
        [<RestEase.Query "q">] q : string *
        [<RestEase.Query "priority_repo_id">] priority_repo_id : int *
        [<RestEase.Query "type">] type' : string *
        [<RestEase.Query "since">] since : string *
        [<RestEase.Query "before">] before : string *
        [<RestEase.Query "assigned">] assigned : bool *
        [<RestEase.Query "created">] created : bool *
        [<RestEase.Query "mentioned">] mentioned : bool *
        [<RestEase.Query "review_requested">] review_requested : bool *
        [<RestEase.Query "owner">] owner : string *
        [<RestEase.Query "team">] team : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Issue list System.Threading.Tasks.Task

    /// Migrate a remote git repository
    [<RestEase.Post "repos/migrate">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoMigrate :
        [<RestEase.Body>] body : MigrateRepoOptions * ?ct : System.Threading.CancellationToken ->
            Repository System.Threading.Tasks.Task

    /// Search for repositories
    [<RestEase.Get "repos/search">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoSearch :
        [<RestEase.Query "q">] q : string *
        [<RestEase.Query "topic">] topic : bool *
        [<RestEase.Query "includeDesc">] includeDesc : bool *
        [<RestEase.Query "uid">] uid : int *
        [<RestEase.Query "priority_owner_id">] priority_owner_id : int *
        [<RestEase.Query "team_id">] team_id : int *
        [<RestEase.Query "starredBy">] starredBy : int *
        [<RestEase.Query "private">] private' : bool *
        [<RestEase.Query "is_private">] is_private : bool *
        [<RestEase.Query "template">] template : bool *
        [<RestEase.Query "archived">] archived : bool *
        [<RestEase.Query "mode">] mode : string *
        [<RestEase.Query "exclusive">] exclusive : bool *
        [<RestEase.Query "sort">] sort : string *
        [<RestEase.Query "order">] order : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            SearchResults System.Threading.Tasks.Task

    /// Get a repository
    [<RestEase.Get "repos/{owner}/{repo}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGet :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            Repository System.Threading.Tasks.Task

    /// Delete a repository
    [<RestEase.Delete "repos/{owner}/{repo}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDelete :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Edit a repository's properties. Only fields that are set will be changed.
    [<RestEase.Post "repos/{owner}/{repo}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoEdit :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : EditRepoOption *
        ?ct : System.Threading.CancellationToken ->
            Repository System.Threading.Tasks.Task

    /// Get an archive of a repository
    [<RestEase.Get "repos/{owner}/{repo}/archive/{archive}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetArchive :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "archive">] archive : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Return all users that have write access and can be assigned to issues
    [<RestEase.Get "repos/{owner}/{repo}/assignees">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetAssignees :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            User list System.Threading.Tasks.Task

    /// List branch protections for a repository
    [<RestEase.Get "repos/{owner}/{repo}/branch_protections">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListBranchProtection :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            BranchProtection list System.Threading.Tasks.Task

    /// Create a branch protections for a repository
    [<RestEase.Post "repos/{owner}/{repo}/branch_protections">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoCreateBranchProtection :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : CreateBranchProtectionOption *
        ?ct : System.Threading.CancellationToken ->
            BranchProtection System.Threading.Tasks.Task

    /// Get a specific branch protection for the repository
    [<RestEase.Get "repos/{owner}/{repo}/branch_protections/{name}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetBranchProtection :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "name">] name : string *
        ?ct : System.Threading.CancellationToken ->
            BranchProtection System.Threading.Tasks.Task

    /// Delete a specific branch protection for the repository
    [<RestEase.Delete "repos/{owner}/{repo}/branch_protections/{name}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeleteBranchProtection :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "name">] name : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Edit a branch protections for a repository. Only fields that are set will be changed
    [<RestEase.Post "repos/{owner}/{repo}/branch_protections/{name}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoEditBranchProtection :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "name">] name : string *
        [<RestEase.Body>] body : EditBranchProtectionOption *
        ?ct : System.Threading.CancellationToken ->
            BranchProtection System.Threading.Tasks.Task

    /// List a repository's branches
    [<RestEase.Get "repos/{owner}/{repo}/branches">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListBranches :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Branch list System.Threading.Tasks.Task

    /// Create a branch
    [<RestEase.Post "repos/{owner}/{repo}/branches">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoCreateBranch :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : CreateBranchRepoOption *
        ?ct : System.Threading.CancellationToken ->
            Branch System.Threading.Tasks.Task

    /// Retrieve a specific branch from a repository, including its effective branch protection
    [<RestEase.Get "repos/{owner}/{repo}/branches/{branch}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetBranch :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "branch">] branch : string *
        ?ct : System.Threading.CancellationToken ->
            Branch System.Threading.Tasks.Task

    /// Delete a specific branch from a repository
    [<RestEase.Delete "repos/{owner}/{repo}/branches/{branch}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeleteBranch :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "branch">] branch : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List a repository's collaborators
    [<RestEase.Get "repos/{owner}/{repo}/collaborators">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListCollaborators :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            User list System.Threading.Tasks.Task

    /// Check if a user is a collaborator of a repository
    [<RestEase.Get "repos/{owner}/{repo}/collaborators/{collaborator}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoCheckCollaborator :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "collaborator">] collaborator : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Delete a collaborator from a repository
    [<RestEase.Delete "repos/{owner}/{repo}/collaborators/{collaborator}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeleteCollaborator :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "collaborator">] collaborator : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Add a collaborator to a repository
    [<RestEase.Put "repos/{owner}/{repo}/collaborators/{collaborator}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoAddCollaborator :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "collaborator">] collaborator : string *
        [<RestEase.Body>] body : AddCollaboratorOption *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get repository permissions for a user
    [<RestEase.Get "repos/{owner}/{repo}/collaborators/{collaborator}/permission">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetRepoPermissions :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "collaborator">] collaborator : string *
        ?ct : System.Threading.CancellationToken ->
            RepoCollaboratorPermission System.Threading.Tasks.Task

    /// Get a list of all commits from a repository
    [<RestEase.Get "repos/{owner}/{repo}/commits">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetAllCommits :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "sha">] sha : string *
        [<RestEase.Query "path">] path : string *
        [<RestEase.Query "stat">] stat : bool *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Commit list System.Threading.Tasks.Task

    /// Get a commit's combined status, by branch/tag/commit reference
    [<RestEase.Get "repos/{owner}/{repo}/commits/{ref}/status">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetCombinedStatusByRef :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "ref">] ref : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            CombinedStatus System.Threading.Tasks.Task

    /// Get a commit's statuses, by branch/tag/commit reference
    [<RestEase.Get "repos/{owner}/{repo}/commits/{ref}/statuses">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListStatusesByRef :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "ref">] ref : string *
        [<RestEase.Query "sort">] sort : string *
        [<RestEase.Query "state">] state : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            CommitStatus list System.Threading.Tasks.Task

    /// Gets the metadata of all the entries of the root dir
    [<RestEase.Get "repos/{owner}/{repo}/contents">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetContentsList :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "ref">] ref : string *
        ?ct : System.Threading.CancellationToken ->
            ContentsResponse list System.Threading.Tasks.Task

    /// Gets the metadata and contents (if a file) of an entry in a repository, or a list of entries if a dir
    [<RestEase.Get "repos/{owner}/{repo}/contents/{filepath}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetContents :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "filepath">] filepath : string *
        [<RestEase.Query "ref">] ref : string *
        ?ct : System.Threading.CancellationToken ->
            ContentsResponse System.Threading.Tasks.Task

    /// Create a file in a repository
    [<RestEase.Post "repos/{owner}/{repo}/contents/{filepath}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoCreateFile :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "filepath">] filepath : string *
        [<RestEase.Body>] body : CreateFileOptions *
        ?ct : System.Threading.CancellationToken ->
            FileResponse System.Threading.Tasks.Task

    /// Delete a file in a repository
    [<RestEase.Delete "repos/{owner}/{repo}/contents/{filepath}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeleteFile :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "filepath">] filepath : string *
        [<RestEase.Body>] body : DeleteFileOptions *
        ?ct : System.Threading.CancellationToken ->
            FileDeleteResponse System.Threading.Tasks.Task

    /// Update a file in a repository
    [<RestEase.Put "repos/{owner}/{repo}/contents/{filepath}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoUpdateFile :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "filepath">] filepath : string *
        [<RestEase.Body>] body : UpdateFileOptions *
        ?ct : System.Threading.CancellationToken ->
            FileResponse System.Threading.Tasks.Task

    /// Apply diff patch to repository
    [<RestEase.Post "repos/{owner}/{repo}/diffpatch">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoApplyDiffPatch :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : UpdateFileOptions *
        ?ct : System.Threading.CancellationToken ->
            FileResponse System.Threading.Tasks.Task

    /// Get the EditorConfig definitions of a file in a repository
    [<RestEase.Get "repos/{owner}/{repo}/editorconfig/{filepath}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetEditorConfig :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "filepath">] filepath : string *
        [<RestEase.Query "ref">] ref : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List a repository's forks
    [<RestEase.Get "repos/{owner}/{repo}/forks">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract ListForks :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Repository list System.Threading.Tasks.Task

    /// Fork a repository
    [<RestEase.Post "repos/{owner}/{repo}/forks">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract CreateFork :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : CreateForkOption *
        ?ct : System.Threading.CancellationToken ->
            Repository System.Threading.Tasks.Task

    /// Gets the blob of a repository.
    [<RestEase.Get "repos/{owner}/{repo}/git/blobs/{sha}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract GetBlob :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "sha">] sha : string *
        ?ct : System.Threading.CancellationToken ->
            GitBlobResponse System.Threading.Tasks.Task

    /// Get a single commit from a repository
    [<RestEase.Get "repos/{owner}/{repo}/git/commits/{sha}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetSingleCommit :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "sha">] sha : string *
        ?ct : System.Threading.CancellationToken ->
            Commit System.Threading.Tasks.Task

    /// Get a commit's diff or patch
    [<RestEase.Get "repos/{owner}/{repo}/git/commits/{sha}.{diffType}">]
    [<RestEase.Header("Content-Type", "plain")>]
    abstract RepoDownloadCommitDiffOrPatch :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "sha">] sha : string *
        [<RestEase.Path "diffType">] diffType : string *
        ?ct : System.Threading.CancellationToken ->
            string System.Threading.Tasks.Task

    /// Get a note corresponding to a single commit from a repository
    [<RestEase.Get "repos/{owner}/{repo}/git/notes/{sha}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetNote :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "sha">] sha : string *
        ?ct : System.Threading.CancellationToken ->
            Note System.Threading.Tasks.Task

    /// Get specified ref or filtered repository's refs
    [<RestEase.Get "repos/{owner}/{repo}/git/refs">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListAllGitRefs :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            Reference list System.Threading.Tasks.Task

    /// Get specified ref or filtered repository's refs
    [<RestEase.Get "repos/{owner}/{repo}/git/refs/{ref}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListGitRefs :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "ref">] ref : string *
        ?ct : System.Threading.CancellationToken ->
            Reference list System.Threading.Tasks.Task

    /// Gets the tag object of an annotated tag (not lightweight tags)
    [<RestEase.Get "repos/{owner}/{repo}/git/tags/{sha}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract GetAnnotatedTag :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "sha">] sha : string *
        ?ct : System.Threading.CancellationToken ->
            AnnotatedTag System.Threading.Tasks.Task

    /// Gets the tree of a repository.
    [<RestEase.Get "repos/{owner}/{repo}/git/trees/{sha}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract GetTree :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "sha">] sha : string *
        [<RestEase.Query "recursive">] recursive : bool *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "per_page">] per_page : int *
        ?ct : System.Threading.CancellationToken ->
            GitTreeResponse System.Threading.Tasks.Task

    /// List the hooks in a repository
    [<RestEase.Get "repos/{owner}/{repo}/hooks">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListHooks :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Hook list System.Threading.Tasks.Task

    /// Create a hook
    [<RestEase.Post "repos/{owner}/{repo}/hooks">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoCreateHook :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : CreateHookOption *
        ?ct : System.Threading.CancellationToken ->
            Hook System.Threading.Tasks.Task

    /// List the Git hooks in a repository
    [<RestEase.Get "repos/{owner}/{repo}/hooks/git">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListGitHooks :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            GitHook list System.Threading.Tasks.Task

    /// Get a Git hook
    [<RestEase.Get "repos/{owner}/{repo}/hooks/git/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetGitHook :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : string *
        ?ct : System.Threading.CancellationToken ->
            GitHook System.Threading.Tasks.Task

    /// Delete a Git hook in a repository
    [<RestEase.Delete "repos/{owner}/{repo}/hooks/git/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeleteGitHook :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Edit a Git hook in a repository
    [<RestEase.Post "repos/{owner}/{repo}/hooks/git/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoEditGitHook :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : string *
        [<RestEase.Body>] body : EditGitHookOption *
        ?ct : System.Threading.CancellationToken ->
            GitHook System.Threading.Tasks.Task

    /// Get a hook
    [<RestEase.Get "repos/{owner}/{repo}/hooks/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetHook :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            Hook System.Threading.Tasks.Task

    /// Delete a hook in a repository
    [<RestEase.Delete "repos/{owner}/{repo}/hooks/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeleteHook :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Edit a hook in a repository
    [<RestEase.Post "repos/{owner}/{repo}/hooks/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoEditHook :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        [<RestEase.Body>] body : EditHookOption *
        ?ct : System.Threading.CancellationToken ->
            Hook System.Threading.Tasks.Task

    /// Test a push webhook
    [<RestEase.Post "repos/{owner}/{repo}/hooks/{id}/tests">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoTestHook :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        [<RestEase.Query "ref">] ref : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get available issue templates for a repository
    [<RestEase.Get "repos/{owner}/{repo}/issue_templates">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetIssueTemplates :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            IssueTemplate list System.Threading.Tasks.Task

    /// List a repository's issues
    [<RestEase.Get "repos/{owner}/{repo}/issues">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueListIssues :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "state">] state : string *
        [<RestEase.Query "labels">] labels : string *
        [<RestEase.Query "q">] q : string *
        [<RestEase.Query "type">] type' : string *
        [<RestEase.Query "milestones">] milestones : string *
        [<RestEase.Query "since">] since : string *
        [<RestEase.Query "before">] before : string *
        [<RestEase.Query "created_by">] created_by : string *
        [<RestEase.Query "assigned_by">] assigned_by : string *
        [<RestEase.Query "mentioned_by">] mentioned_by : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Issue list System.Threading.Tasks.Task

    /// Create an issue. If using deadline only the date will be taken into account, and time of day ignored.
    [<RestEase.Post "repos/{owner}/{repo}/issues">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueCreateIssue :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : CreateIssueOption *
        ?ct : System.Threading.CancellationToken ->
            Issue System.Threading.Tasks.Task

    /// List all comments in a repository
    [<RestEase.Get "repos/{owner}/{repo}/issues/comments">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueGetRepoComments :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "since">] since : string *
        [<RestEase.Query "before">] before : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Comment list System.Threading.Tasks.Task

    /// Delete a comment
    [<RestEase.Delete "repos/{owner}/{repo}/issues/comments/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueDeleteComment :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List comment's attachments
    [<RestEase.Get "repos/{owner}/{repo}/issues/comments/{id}/assets">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueListIssueCommentAttachments :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            Attachment list System.Threading.Tasks.Task

    /// Get a comment attachment
    [<RestEase.Get "repos/{owner}/{repo}/issues/comments/{id}/assets/{attachment_id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueGetIssueCommentAttachment :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        [<RestEase.Path "attachment_id">] attachment_id : int *
        ?ct : System.Threading.CancellationToken ->
            Attachment System.Threading.Tasks.Task

    /// Delete a comment attachment
    [<RestEase.Delete "repos/{owner}/{repo}/issues/comments/{id}/assets/{attachment_id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueDeleteIssueCommentAttachment :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        [<RestEase.Path "attachment_id">] attachment_id : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Edit a comment attachment
    [<RestEase.Post "repos/{owner}/{repo}/issues/comments/{id}/assets/{attachment_id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueEditIssueCommentAttachment :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        [<RestEase.Path "attachment_id">] attachment_id : int *
        [<RestEase.Body>] body : EditAttachmentOptions *
        ?ct : System.Threading.CancellationToken ->
            Attachment System.Threading.Tasks.Task

    /// Get a list of reactions from a comment of an issue
    [<RestEase.Get "repos/{owner}/{repo}/issues/comments/{id}/reactions">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueGetCommentReactions :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            Reaction list System.Threading.Tasks.Task

    /// Remove a reaction from a comment of an issue
    [<RestEase.Delete "repos/{owner}/{repo}/issues/comments/{id}/reactions">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueDeleteCommentReaction :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        [<RestEase.Body>] content : EditReactionOption *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get an issue
    [<RestEase.Get "repos/{owner}/{repo}/issues/{index}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueGetIssue :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        ?ct : System.Threading.CancellationToken ->
            Issue System.Threading.Tasks.Task

    /// Delete an issue
    [<RestEase.Delete "repos/{owner}/{repo}/issues/{index}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueDelete :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Edit an issue. If using deadline only the date will be taken into account, and time of day ignored.
    [<RestEase.Post "repos/{owner}/{repo}/issues/{index}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueEditIssue :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Body>] body : EditIssueOption *
        ?ct : System.Threading.CancellationToken ->
            Issue System.Threading.Tasks.Task

    /// List issue's attachments
    [<RestEase.Get "repos/{owner}/{repo}/issues/{index}/assets">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueListIssueAttachments :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        ?ct : System.Threading.CancellationToken ->
            Attachment list System.Threading.Tasks.Task

    /// Get an issue attachment
    [<RestEase.Get "repos/{owner}/{repo}/issues/{index}/assets/{attachment_id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueGetIssueAttachment :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Path "attachment_id">] attachment_id : int *
        ?ct : System.Threading.CancellationToken ->
            Attachment System.Threading.Tasks.Task

    /// Delete an issue attachment
    [<RestEase.Delete "repos/{owner}/{repo}/issues/{index}/assets/{attachment_id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueDeleteIssueAttachment :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Path "attachment_id">] attachment_id : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Edit an issue attachment
    [<RestEase.Post "repos/{owner}/{repo}/issues/{index}/assets/{attachment_id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueEditIssueAttachment :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Path "attachment_id">] attachment_id : int *
        [<RestEase.Body>] body : EditAttachmentOptions *
        ?ct : System.Threading.CancellationToken ->
            Attachment System.Threading.Tasks.Task

    /// List all comments on an issue
    [<RestEase.Get "repos/{owner}/{repo}/issues/{index}/comments">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueGetComments :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Query "since">] since : string *
        [<RestEase.Query "before">] before : string *
        ?ct : System.Threading.CancellationToken ->
            Comment list System.Threading.Tasks.Task

    /// Add a comment to an issue
    [<RestEase.Post "repos/{owner}/{repo}/issues/{index}/comments">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueCreateComment :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Body>] body : CreateIssueCommentOption *
        ?ct : System.Threading.CancellationToken ->
            Comment System.Threading.Tasks.Task

    /// Delete a comment
    [<RestEase.Delete "repos/{owner}/{repo}/issues/{index}/comments/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueDeleteCommentDeprecated :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Set an issue deadline. If set to null, the deadline is deleted. If using deadline only the date will be taken into account, and time of day ignored.
    [<RestEase.Post "repos/{owner}/{repo}/issues/{index}/deadline">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueEditIssueDeadline :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Body>] body : EditDeadlineOption *
        ?ct : System.Threading.CancellationToken ->
            IssueDeadline System.Threading.Tasks.Task

    /// Get an issue's labels
    [<RestEase.Get "repos/{owner}/{repo}/issues/{index}/labels">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueGetLabels :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        ?ct : System.Threading.CancellationToken ->
            Label list System.Threading.Tasks.Task

    /// Add a label to an issue
    [<RestEase.Post "repos/{owner}/{repo}/issues/{index}/labels">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueAddLabel :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Body>] body : IssueLabelsOption *
        ?ct : System.Threading.CancellationToken ->
            Label list System.Threading.Tasks.Task

    /// Remove all labels from an issue
    [<RestEase.Delete "repos/{owner}/{repo}/issues/{index}/labels">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueClearLabels :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Replace an issue's labels
    [<RestEase.Put "repos/{owner}/{repo}/issues/{index}/labels">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueReplaceLabels :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Body>] body : IssueLabelsOption *
        ?ct : System.Threading.CancellationToken ->
            Label list System.Threading.Tasks.Task

    /// Remove a label from an issue
    [<RestEase.Delete "repos/{owner}/{repo}/issues/{index}/labels/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueRemoveLabel :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get a list reactions of an issue
    [<RestEase.Get "repos/{owner}/{repo}/issues/{index}/reactions">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueGetIssueReactions :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Reaction list System.Threading.Tasks.Task

    /// Remove a reaction from an issue
    [<RestEase.Delete "repos/{owner}/{repo}/issues/{index}/reactions">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueDeleteIssueReaction :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Body>] content : EditReactionOption *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Delete an issue's existing stopwatch.
    [<RestEase.Delete "repos/{owner}/{repo}/issues/{index}/stopwatch/delete">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueDeleteStopWatch :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Start stopwatch on an issue.
    [<RestEase.Post "repos/{owner}/{repo}/issues/{index}/stopwatch/start">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueStartStopWatch :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Stop an issue's existing stopwatch.
    [<RestEase.Post "repos/{owner}/{repo}/issues/{index}/stopwatch/stop">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueStopStopWatch :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get users who subscribed on an issue.
    [<RestEase.Get "repos/{owner}/{repo}/issues/{index}/subscriptions">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueSubscriptions :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            User list System.Threading.Tasks.Task

    /// Check if user is subscribed to an issue
    [<RestEase.Get "repos/{owner}/{repo}/issues/{index}/subscriptions/check">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueCheckSubscription :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        ?ct : System.Threading.CancellationToken ->
            WatchInfo System.Threading.Tasks.Task

    /// List all comments and events on an issue
    [<RestEase.Get "repos/{owner}/{repo}/issues/{index}/timeline">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueGetCommentsAndTimeline :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Query "since">] since : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        [<RestEase.Query "before">] before : string *
        ?ct : System.Threading.CancellationToken ->
            TimelineComment list System.Threading.Tasks.Task

    /// List an issue's tracked times
    [<RestEase.Get "repos/{owner}/{repo}/issues/{index}/times">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueTrackedTimes :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Query "user">] user : string *
        [<RestEase.Query "since">] since : string *
        [<RestEase.Query "before">] before : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            TrackedTime list System.Threading.Tasks.Task

    /// Add tracked time to a issue
    [<RestEase.Post "repos/{owner}/{repo}/issues/{index}/times">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueAddTime :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Body>] body : AddTimeOption *
        ?ct : System.Threading.CancellationToken ->
            TrackedTime System.Threading.Tasks.Task

    /// Reset a tracked time of an issue
    [<RestEase.Delete "repos/{owner}/{repo}/issues/{index}/times">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueResetTime :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Delete specific tracked time
    [<RestEase.Delete "repos/{owner}/{repo}/issues/{index}/times/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueDeleteTime :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List a repository's keys
    [<RestEase.Get "repos/{owner}/{repo}/keys">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListKeys :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "key_id">] key_id : int *
        [<RestEase.Query "fingerprint">] fingerprint : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            DeployKey list System.Threading.Tasks.Task

    /// Add a key to a repository
    [<RestEase.Post "repos/{owner}/{repo}/keys">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoCreateKey :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : CreateKeyOption *
        ?ct : System.Threading.CancellationToken ->
            DeployKey System.Threading.Tasks.Task

    /// Get a repository's key by id
    [<RestEase.Get "repos/{owner}/{repo}/keys/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetKey :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            DeployKey System.Threading.Tasks.Task

    /// Delete a key from a repository
    [<RestEase.Delete "repos/{owner}/{repo}/keys/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeleteKey :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get all of a repository's labels
    [<RestEase.Get "repos/{owner}/{repo}/labels">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueListLabels :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Label list System.Threading.Tasks.Task

    /// Create a label
    [<RestEase.Post "repos/{owner}/{repo}/labels">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueCreateLabel :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : CreateLabelOption *
        ?ct : System.Threading.CancellationToken ->
            Label System.Threading.Tasks.Task

    /// Get a single label
    [<RestEase.Get "repos/{owner}/{repo}/labels/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueGetLabel :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            Label System.Threading.Tasks.Task

    /// Delete a label
    [<RestEase.Delete "repos/{owner}/{repo}/labels/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueDeleteLabel :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Update a label
    [<RestEase.Post "repos/{owner}/{repo}/labels/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueEditLabel :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        [<RestEase.Body>] body : EditLabelOption *
        ?ct : System.Threading.CancellationToken ->
            Label System.Threading.Tasks.Task

    /// Get languages and number of bytes of code written
    [<RestEase.Get "repos/{owner}/{repo}/languages">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetLanguages :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            LanguageStatistics System.Threading.Tasks.Task

    /// Get a file or it's LFS object from a repository
    [<RestEase.Get "repos/{owner}/{repo}/media/{filepath}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetRawFileOrLFS :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "filepath">] filepath : string *
        [<RestEase.Query "ref">] ref : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get all of a repository's opened milestones
    [<RestEase.Get "repos/{owner}/{repo}/milestones">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueGetMilestonesList :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "state">] state : string *
        [<RestEase.Query "name">] name : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Milestone list System.Threading.Tasks.Task

    /// Create a milestone
    [<RestEase.Post "repos/{owner}/{repo}/milestones">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueCreateMilestone :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : CreateMilestoneOption *
        ?ct : System.Threading.CancellationToken ->
            Milestone System.Threading.Tasks.Task

    /// Get a milestone
    [<RestEase.Get "repos/{owner}/{repo}/milestones/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueGetMilestone :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : string *
        ?ct : System.Threading.CancellationToken ->
            Milestone System.Threading.Tasks.Task

    /// Delete a milestone
    [<RestEase.Delete "repos/{owner}/{repo}/milestones/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueDeleteMilestone :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Update a milestone
    [<RestEase.Post "repos/{owner}/{repo}/milestones/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract IssueEditMilestone :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : string *
        [<RestEase.Body>] body : EditMilestoneOption *
        ?ct : System.Threading.CancellationToken ->
            Milestone System.Threading.Tasks.Task

    /// Sync a mirrored repository
    [<RestEase.Post "repos/{owner}/{repo}/mirror-sync">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoMirrorSync :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List users's notification threads on a specific repo
    [<RestEase.Get "repos/{owner}/{repo}/notifications">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract NotifyGetRepoList :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "all">] all : bool *
        [<RestEase.Query "status-types">] status_types : string list *
        [<RestEase.Query "subject-type">] subject_type : string list *
        [<RestEase.Query "since">] since : string *
        [<RestEase.Query "before">] before : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            NotificationThread list System.Threading.Tasks.Task

    /// Mark notification threads as read, pinned or unread on a specific repo
    [<RestEase.Put "repos/{owner}/{repo}/notifications">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract NotifyReadRepoList :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "all">] all : string *
        [<RestEase.Query "status-types">] status_types : string list *
        [<RestEase.Query "to-status">] to_status : string *
        [<RestEase.Query "last_read_at">] last_read_at : string *
        ?ct : System.Threading.CancellationToken ->
            NotificationThread list System.Threading.Tasks.Task

    /// List a repo's pull requests
    [<RestEase.Get "repos/{owner}/{repo}/pulls">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListPullRequests :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "state">] state : string *
        [<RestEase.Query "sort">] sort : string *
        [<RestEase.Query "milestone">] milestone : int *
        [<RestEase.Query "labels">] labels : int list *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            PullRequest list System.Threading.Tasks.Task

    /// Create a pull request
    [<RestEase.Post "repos/{owner}/{repo}/pulls">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoCreatePullRequest :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : CreatePullRequestOption *
        ?ct : System.Threading.CancellationToken ->
            PullRequest System.Threading.Tasks.Task

    /// Get a pull request
    [<RestEase.Get "repos/{owner}/{repo}/pulls/{index}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetPullRequest :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        ?ct : System.Threading.CancellationToken ->
            PullRequest System.Threading.Tasks.Task

    /// Update a pull request. If using deadline only the date will be taken into account, and time of day ignored.
    [<RestEase.Post "repos/{owner}/{repo}/pulls/{index}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoEditPullRequest :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Body>] body : EditPullRequestOption *
        ?ct : System.Threading.CancellationToken ->
            PullRequest System.Threading.Tasks.Task

    /// Get a pull request diff or patch
    [<RestEase.Get "repos/{owner}/{repo}/pulls/{index}.{diffType}">]
    [<RestEase.Header("Content-Type", "plain")>]
    abstract RepoDownloadPullDiffOrPatch :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Path "diffType">] diffType : string *
        [<RestEase.Query "binary">] binary : bool *
        ?ct : System.Threading.CancellationToken ->
            string System.Threading.Tasks.Task

    /// Get commits for a pull request
    [<RestEase.Get "repos/{owner}/{repo}/pulls/{index}/commits">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetPullRequestCommits :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Commit list System.Threading.Tasks.Task

    /// Get changed files for a pull request
    [<RestEase.Get "repos/{owner}/{repo}/pulls/{index}/files">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetPullRequestFiles :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Query "skip-to">] skip_to : string *
        [<RestEase.Query "whitespace">] whitespace : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            ChangedFile list System.Threading.Tasks.Task

    /// Check if a pull request has been merged
    [<RestEase.Get "repos/{owner}/{repo}/pulls/{index}/merge">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoPullRequestIsMerged :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Merge a pull request
    [<RestEase.Post "repos/{owner}/{repo}/pulls/{index}/merge">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoMergePullRequest :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Body>] body : MergePullRequestOption *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Cancel the scheduled auto merge for the given pull request
    [<RestEase.Delete "repos/{owner}/{repo}/pulls/{index}/merge">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoCancelScheduledAutoMerge :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// create review requests for a pull request
    [<RestEase.Post "repos/{owner}/{repo}/pulls/{index}/requested_reviewers">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoCreatePullReviewRequests :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Body>] body : PullReviewRequestOptions *
        ?ct : System.Threading.CancellationToken ->
            PullReview list System.Threading.Tasks.Task

    /// cancel review requests for a pull request
    [<RestEase.Delete "repos/{owner}/{repo}/pulls/{index}/requested_reviewers">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeletePullReviewRequests :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Body>] body : PullReviewRequestOptions *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List all reviews for a pull request
    [<RestEase.Get "repos/{owner}/{repo}/pulls/{index}/reviews">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListPullReviews :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            PullReview list System.Threading.Tasks.Task

    /// Create a review to an pull request
    [<RestEase.Post "repos/{owner}/{repo}/pulls/{index}/reviews">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoCreatePullReview :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Body>] body : CreatePullReviewOptions *
        ?ct : System.Threading.CancellationToken ->
            PullReview System.Threading.Tasks.Task

    /// Get a specific review for a pull request
    [<RestEase.Get "repos/{owner}/{repo}/pulls/{index}/reviews/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetPullReview :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            PullReview System.Threading.Tasks.Task

    /// Submit a pending review to an pull request
    [<RestEase.Post "repos/{owner}/{repo}/pulls/{index}/reviews/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoSubmitPullReview :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Path "id">] id : int *
        [<RestEase.Body>] body : SubmitPullReviewOptions *
        ?ct : System.Threading.CancellationToken ->
            PullReview System.Threading.Tasks.Task

    /// Delete a specific review from a pull request
    [<RestEase.Delete "repos/{owner}/{repo}/pulls/{index}/reviews/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeletePullReview :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get a specific review for a pull request
    [<RestEase.Get "repos/{owner}/{repo}/pulls/{index}/reviews/{id}/comments">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetPullReviewComments :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            PullReviewComment list System.Threading.Tasks.Task

    /// Dismiss a review for a pull request
    [<RestEase.Post "repos/{owner}/{repo}/pulls/{index}/reviews/{id}/dismissals">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDismissPullReview :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Path "id">] id : int *
        [<RestEase.Body>] body : DismissPullReviewOptions *
        ?ct : System.Threading.CancellationToken ->
            PullReview System.Threading.Tasks.Task

    /// Cancel to dismiss a review for a pull request
    [<RestEase.Post "repos/{owner}/{repo}/pulls/{index}/reviews/{id}/undismissals">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoUnDismissPullReview :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            PullReview System.Threading.Tasks.Task

    /// Merge PR's baseBranch into headBranch
    [<RestEase.Post "repos/{owner}/{repo}/pulls/{index}/update">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoUpdatePullRequest :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "index">] index : int *
        [<RestEase.Query "style">] style : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get all push mirrors of the repository
    [<RestEase.Get "repos/{owner}/{repo}/push_mirrors">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListPushMirrors :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            PushMirror list System.Threading.Tasks.Task

    /// add a push mirror to the repository
    [<RestEase.Post "repos/{owner}/{repo}/push_mirrors">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoAddPushMirror :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : CreatePushMirrorOption *
        ?ct : System.Threading.CancellationToken ->
            PushMirror System.Threading.Tasks.Task

    /// Sync all push mirrored repository
    [<RestEase.Post "repos/{owner}/{repo}/push_mirrors-sync">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoPushMirrorSync :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get push mirror of the repository by remoteName
    [<RestEase.Get "repos/{owner}/{repo}/push_mirrors/{name}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetPushMirrorByRemoteName :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "name">] name : string *
        ?ct : System.Threading.CancellationToken ->
            PushMirror System.Threading.Tasks.Task

    /// deletes a push mirror from a repository by remoteName
    [<RestEase.Delete "repos/{owner}/{repo}/push_mirrors/{name}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeletePushMirror :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "name">] name : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get a file from a repository
    [<RestEase.Get "repos/{owner}/{repo}/raw/{filepath}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetRawFile :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "filepath">] filepath : string *
        [<RestEase.Query "ref">] ref : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List a repo's releases
    [<RestEase.Get "repos/{owner}/{repo}/releases">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListReleases :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "draft">] draft : bool *
        [<RestEase.Query "pre-release">] pre_release : bool *
        [<RestEase.Query "per_page">] per_page : int *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Release list System.Threading.Tasks.Task

    /// Create a release
    [<RestEase.Post "repos/{owner}/{repo}/releases">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoCreateRelease :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : CreateReleaseOption *
        ?ct : System.Threading.CancellationToken ->
            Release System.Threading.Tasks.Task

    /// Gets the most recent non-prerelease, non-draft release of a repository, sorted by created_at
    [<RestEase.Get "repos/{owner}/{repo}/releases/latest">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetLatestRelease :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            Release System.Threading.Tasks.Task

    /// Get a release by tag name
    [<RestEase.Get "repos/{owner}/{repo}/releases/tags/{tag}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetReleaseByTag :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "tag">] tag : string *
        ?ct : System.Threading.CancellationToken ->
            Release System.Threading.Tasks.Task

    /// Delete a release by tag name
    [<RestEase.Delete "repos/{owner}/{repo}/releases/tags/{tag}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeleteReleaseByTag :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "tag">] tag : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get a release
    [<RestEase.Get "repos/{owner}/{repo}/releases/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetRelease :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            Release System.Threading.Tasks.Task

    /// Delete a release
    [<RestEase.Delete "repos/{owner}/{repo}/releases/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeleteRelease :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Update a release
    [<RestEase.Post "repos/{owner}/{repo}/releases/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoEditRelease :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        [<RestEase.Body>] body : EditReleaseOption *
        ?ct : System.Threading.CancellationToken ->
            Release System.Threading.Tasks.Task

    /// List release's attachments
    [<RestEase.Get "repos/{owner}/{repo}/releases/{id}/assets">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListReleaseAttachments :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        ?ct : System.Threading.CancellationToken ->
            Attachment list System.Threading.Tasks.Task

    /// Get a release attachment
    [<RestEase.Get "repos/{owner}/{repo}/releases/{id}/assets/{attachment_id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetReleaseAttachment :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        [<RestEase.Path "attachment_id">] attachment_id : int *
        ?ct : System.Threading.CancellationToken ->
            Attachment System.Threading.Tasks.Task

    /// Delete a release attachment
    [<RestEase.Delete "repos/{owner}/{repo}/releases/{id}/assets/{attachment_id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeleteReleaseAttachment :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        [<RestEase.Path "attachment_id">] attachment_id : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Edit a release attachment
    [<RestEase.Post "repos/{owner}/{repo}/releases/{id}/assets/{attachment_id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoEditReleaseAttachment :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "id">] id : int *
        [<RestEase.Path "attachment_id">] attachment_id : int *
        [<RestEase.Body>] body : EditAttachmentOptions *
        ?ct : System.Threading.CancellationToken ->
            Attachment System.Threading.Tasks.Task

    /// Return all users that can be requested to review in this repo
    [<RestEase.Get "repos/{owner}/{repo}/reviewers">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetReviewers :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            User list System.Threading.Tasks.Task

    /// Get signing-key.gpg for given repository
    [<RestEase.Get "repos/{owner}/{repo}/signing-key.gpg">]
    [<RestEase.Header("Content-Type", "plain")>]
    abstract RepoSigningKey :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List a repo's stargazers
    [<RestEase.Get "repos/{owner}/{repo}/stargazers">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListStargazers :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            User list System.Threading.Tasks.Task

    /// Get a commit's statuses
    [<RestEase.Get "repos/{owner}/{repo}/statuses/{sha}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListStatuses :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "sha">] sha : string *
        [<RestEase.Query "sort">] sort : string *
        [<RestEase.Query "state">] state : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            CommitStatus list System.Threading.Tasks.Task

    /// Create a commit status
    [<RestEase.Post "repos/{owner}/{repo}/statuses/{sha}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoCreateStatus :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "sha">] sha : string *
        [<RestEase.Body>] body : CreateStatusOption *
        ?ct : System.Threading.CancellationToken ->
            CommitStatus System.Threading.Tasks.Task

    /// List a repo's watchers
    [<RestEase.Get "repos/{owner}/{repo}/subscribers">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListSubscribers :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            User list System.Threading.Tasks.Task

    /// Check if the current user is watching a repo
    [<RestEase.Get "repos/{owner}/{repo}/subscription">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentCheckSubscription :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            WatchInfo System.Threading.Tasks.Task

    /// Unwatch a repo
    [<RestEase.Delete "repos/{owner}/{repo}/subscription">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentDeleteSubscription :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Watch a repo
    [<RestEase.Put "repos/{owner}/{repo}/subscription">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentPutSubscription :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            WatchInfo System.Threading.Tasks.Task

    /// List a repository's tags
    [<RestEase.Get "repos/{owner}/{repo}/tags">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListTags :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Tag list System.Threading.Tasks.Task

    /// Create a new git tag in a repository
    [<RestEase.Post "repos/{owner}/{repo}/tags">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoCreateTag :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : CreateTagOption *
        ?ct : System.Threading.CancellationToken ->
            Tag System.Threading.Tasks.Task

    /// Get the tag of a repository by tag name
    [<RestEase.Get "repos/{owner}/{repo}/tags/{tag}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetTag :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "tag">] tag : string *
        ?ct : System.Threading.CancellationToken ->
            Tag System.Threading.Tasks.Task

    /// Delete a repository's tag by name
    [<RestEase.Delete "repos/{owner}/{repo}/tags/{tag}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeleteTag :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "tag">] tag : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List a repository's teams
    [<RestEase.Get "repos/{owner}/{repo}/teams">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListTeams :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            Team list System.Threading.Tasks.Task

    /// Check if a team is assigned to a repository
    [<RestEase.Get "repos/{owner}/{repo}/teams/{team}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoCheckTeam :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "team">] team : string *
        ?ct : System.Threading.CancellationToken ->
            Team System.Threading.Tasks.Task

    /// Delete a team from a repository
    [<RestEase.Delete "repos/{owner}/{repo}/teams/{team}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeleteTeam :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "team">] team : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Add a team to a repository
    [<RestEase.Put "repos/{owner}/{repo}/teams/{team}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoAddTeam :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "team">] team : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List a repo's tracked times
    [<RestEase.Get "repos/{owner}/{repo}/times">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoTrackedTimes :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "user">] user : string *
        [<RestEase.Query "since">] since : string *
        [<RestEase.Query "before">] before : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            TrackedTime list System.Threading.Tasks.Task

    /// List a user's tracked times in a repo
    [<RestEase.Get "repos/{owner}/{repo}/times/{user}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserTrackedTimes :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "user">] user : string *
        ?ct : System.Threading.CancellationToken ->
            TrackedTime list System.Threading.Tasks.Task

    /// Get list of topics that a repository has
    [<RestEase.Get "repos/{owner}/{repo}/topics">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoListTopics :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            TopicName System.Threading.Tasks.Task

    /// Replace list of topics for a repository
    [<RestEase.Put "repos/{owner}/{repo}/topics">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoUpdateTopics :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : RepoTopicOptions *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Delete a topic from a repository
    [<RestEase.Delete "repos/{owner}/{repo}/topics/{topic}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeleteTopic :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "topic">] topic : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Add a topic to a repository
    [<RestEase.Put "repos/{owner}/{repo}/topics/{topic}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoAddTopic :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "topic">] topic : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Transfer a repo ownership
    [<RestEase.Post "repos/{owner}/{repo}/transfer">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoTransfer :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : TransferRepoOption *
        ?ct : System.Threading.CancellationToken ->
            Repository System.Threading.Tasks.Task

    /// Accept a repo transfer
    [<RestEase.Post "repos/{owner}/{repo}/transfer/accept">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract AcceptRepoTransfer :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            Repository System.Threading.Tasks.Task

    /// Reject a repo transfer
    [<RestEase.Post "repos/{owner}/{repo}/transfer/reject">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RejectRepoTransfer :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            Repository System.Threading.Tasks.Task

    /// Create a wiki page
    [<RestEase.Post "repos/{owner}/{repo}/wiki/new">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoCreateWikiPage :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Body>] body : CreateWikiPageOptions *
        ?ct : System.Threading.CancellationToken ->
            WikiPage System.Threading.Tasks.Task

    /// Get a wiki page
    [<RestEase.Get "repos/{owner}/{repo}/wiki/page/{pageName}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetWikiPage :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "pageName">] pageName : string *
        ?ct : System.Threading.CancellationToken ->
            WikiPage System.Threading.Tasks.Task

    /// Delete a wiki page
    [<RestEase.Delete "repos/{owner}/{repo}/wiki/page/{pageName}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoDeleteWikiPage :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "pageName">] pageName : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Edit a wiki page
    [<RestEase.Post "repos/{owner}/{repo}/wiki/page/{pageName}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoEditWikiPage :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "pageName">] pageName : string *
        [<RestEase.Body>] body : CreateWikiPageOptions *
        ?ct : System.Threading.CancellationToken ->
            WikiPage System.Threading.Tasks.Task

    /// Get all wiki pages
    [<RestEase.Get "repos/{owner}/{repo}/wiki/pages">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetWikiPages :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            WikiPageMetaData list System.Threading.Tasks.Task

    /// Get revisions of a wiki page
    [<RestEase.Get "repos/{owner}/{repo}/wiki/revisions/{pageName}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetWikiPageRevisions :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        [<RestEase.Path "pageName">] pageName : string *
        [<RestEase.Query "page">] page : int *
        ?ct : System.Threading.CancellationToken ->
            WikiCommitList System.Threading.Tasks.Task

    /// Create a repository using a template
    [<RestEase.Post "repos/{template_owner}/{template_repo}/generate">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract GenerateRepo :
        [<RestEase.Path "template_owner">] template_owner : string *
        [<RestEase.Path "template_repo">] template_repo : string *
        [<RestEase.Body>] body : GenerateRepoOption *
        ?ct : System.Threading.CancellationToken ->
            Repository System.Threading.Tasks.Task

    /// Get a repository by id
    [<RestEase.Get "repositories/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract RepoGetByID :
        [<RestEase.Path "id">] id : int * ?ct : System.Threading.CancellationToken ->
            Repository System.Threading.Tasks.Task

    /// Get instance's global settings for api
    [<RestEase.Get "settings/api">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract GetGeneralAPISettings :
        ?ct : System.Threading.CancellationToken -> GeneralAPISettings System.Threading.Tasks.Task

    /// Get instance's global settings for Attachment
    [<RestEase.Get "settings/attachment">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract GetGeneralAttachmentSettings :
        ?ct : System.Threading.CancellationToken -> GeneralAttachmentSettings System.Threading.Tasks.Task

    /// Get instance's global settings for repositories
    [<RestEase.Get "settings/repository">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract GetGeneralRepositorySettings :
        ?ct : System.Threading.CancellationToken -> GeneralRepoSettings System.Threading.Tasks.Task

    /// Get instance's global settings for ui
    [<RestEase.Get "settings/ui">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract GetGeneralUISettings :
        ?ct : System.Threading.CancellationToken -> GeneralUISettings System.Threading.Tasks.Task

    /// Get default signing-key.gpg
    [<RestEase.Get "signing-key.gpg">]
    [<RestEase.Header("Content-Type", "plain")>]
    abstract GetSigningKey : ?ct : System.Threading.CancellationToken -> unit System.Threading.Tasks.Task

    /// Get a team
    [<RestEase.Get "teams/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgGetTeam :
        [<RestEase.Path "id">] id : int * ?ct : System.Threading.CancellationToken -> Team System.Threading.Tasks.Task

    /// Delete a team
    [<RestEase.Delete "teams/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgDeleteTeam :
        [<RestEase.Path "id">] id : int * ?ct : System.Threading.CancellationToken -> unit System.Threading.Tasks.Task

    /// Edit a team
    [<RestEase.Post "teams/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgEditTeam :
        [<RestEase.Path "id">] id : int *
        [<RestEase.Body>] body : EditTeamOption *
        ?ct : System.Threading.CancellationToken ->
            Team System.Threading.Tasks.Task

    /// List a team's members
    [<RestEase.Get "teams/{id}/members">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgListTeamMembers :
        [<RestEase.Path "id">] id : int *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            User list System.Threading.Tasks.Task

    /// List a particular member of team
    [<RestEase.Get "teams/{id}/members/{username}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgListTeamMember :
        [<RestEase.Path "id">] id : int *
        [<RestEase.Path "username">] username : string *
        ?ct : System.Threading.CancellationToken ->
            User System.Threading.Tasks.Task

    /// Remove a team member
    [<RestEase.Delete "teams/{id}/members/{username}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgRemoveTeamMember :
        [<RestEase.Path "id">] id : int *
        [<RestEase.Path "username">] username : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Add a team member
    [<RestEase.Put "teams/{id}/members/{username}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgAddTeamMember :
        [<RestEase.Path "id">] id : int *
        [<RestEase.Path "username">] username : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List a team's repos
    [<RestEase.Get "teams/{id}/repos">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgListTeamRepos :
        [<RestEase.Path "id">] id : int *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Repository list System.Threading.Tasks.Task

    /// List a particular repo of team
    [<RestEase.Get "teams/{id}/repos/{org}/{repo}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgListTeamRepo :
        [<RestEase.Path "id">] id : int *
        [<RestEase.Path "org">] org : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            Repository System.Threading.Tasks.Task

    /// Remove a repository from a team
    [<RestEase.Delete "teams/{id}/repos/{org}/{repo}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgRemoveTeamRepository :
        [<RestEase.Path "id">] id : int *
        [<RestEase.Path "org">] org : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Add a repository to a team
    [<RestEase.Put "teams/{id}/repos/{org}/{repo}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgAddTeamRepository :
        [<RestEase.Path "id">] id : int *
        [<RestEase.Path "org">] org : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// search topics via keyword
    [<RestEase.Get "topics/search">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract TopicSearch :
        [<RestEase.Query "q">] q : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            TopicResponse list System.Threading.Tasks.Task

    /// Get the authenticated user
    [<RestEase.Get "user">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserGetCurrent : ?ct : System.Threading.CancellationToken -> User System.Threading.Tasks.Task

    /// List the authenticated user's oauth2 applications
    [<RestEase.Get "user/applications/oauth2">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserGetOauth2Application :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            OAuth2Application list System.Threading.Tasks.Task

    /// creates a new OAuth2 application
    [<RestEase.Post "user/applications/oauth2">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCreateOAuth2Application :
        [<RestEase.Body>] body : CreateOAuth2ApplicationOptions * ?ct : System.Threading.CancellationToken ->
            OAuth2Application System.Threading.Tasks.Task

    /// get an OAuth2 Application
    [<RestEase.Get "user/applications/oauth2/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserGetOAuth2Application :
        [<RestEase.Path "id">] id : int * ?ct : System.Threading.CancellationToken ->
            OAuth2Application System.Threading.Tasks.Task

    /// delete an OAuth2 Application
    [<RestEase.Delete "user/applications/oauth2/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserDeleteOAuth2Application :
        [<RestEase.Path "id">] id : int * ?ct : System.Threading.CancellationToken -> unit System.Threading.Tasks.Task

    /// update an OAuth2 Application, this includes regenerating the client secret
    [<RestEase.Post "user/applications/oauth2/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserUpdateOAuth2Application :
        [<RestEase.Path "id">] id : int *
        [<RestEase.Body>] body : CreateOAuth2ApplicationOptions *
        ?ct : System.Threading.CancellationToken ->
            OAuth2Application System.Threading.Tasks.Task

    /// List the authenticated user's email addresses
    [<RestEase.Get "user/emails">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserListEmails : ?ct : System.Threading.CancellationToken -> Email list System.Threading.Tasks.Task

    /// Add email addresses
    [<RestEase.Post "user/emails">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserAddEmail :
        [<RestEase.Body>] body : CreateEmailOption * ?ct : System.Threading.CancellationToken ->
            Email list System.Threading.Tasks.Task

    /// Delete email addresses
    [<RestEase.Delete "user/emails">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserDeleteEmail :
        [<RestEase.Body>] body : DeleteEmailOption * ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// List the authenticated user's followers
    [<RestEase.Get "user/followers">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentListFollowers :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            User list System.Threading.Tasks.Task

    /// List the users that the authenticated user is following
    [<RestEase.Get "user/following">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentListFollowing :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            User list System.Threading.Tasks.Task

    /// Check whether a user is followed by the authenticated user
    [<RestEase.Get "user/following/{username}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentCheckFollowing :
        [<RestEase.Path "username">] username : string * ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Unfollow a user
    [<RestEase.Delete "user/following/{username}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentDeleteFollow :
        [<RestEase.Path "username">] username : string * ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Follow a user
    [<RestEase.Put "user/following/{username}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentPutFollow :
        [<RestEase.Path "username">] username : string * ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get a Token to verify
    [<RestEase.Get "user/gpg_key_token">]
    [<RestEase.Header("Content-Type", "plain")>]
    abstract GetVerificationToken : ?ct : System.Threading.CancellationToken -> string System.Threading.Tasks.Task

    /// Remove a GPG key
    [<RestEase.Delete "user/gpg_keys/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentDeleteGPGKey :
        [<RestEase.Path "id">] id : int * ?ct : System.Threading.CancellationToken -> unit System.Threading.Tasks.Task

    /// List the authenticated user's public keys
    [<RestEase.Get "user/keys">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentListKeys :
        [<RestEase.Query "fingerprint">] fingerprint : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            PublicKey list System.Threading.Tasks.Task

    /// Create a public key
    [<RestEase.Post "user/keys">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentPostKey :
        [<RestEase.Body>] body : CreateKeyOption * ?ct : System.Threading.CancellationToken ->
            PublicKey System.Threading.Tasks.Task

    /// Get a public key
    [<RestEase.Get "user/keys/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentGetKey :
        [<RestEase.Path "id">] id : int * ?ct : System.Threading.CancellationToken ->
            PublicKey System.Threading.Tasks.Task

    /// Delete a public key
    [<RestEase.Delete "user/keys/{id}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentDeleteKey :
        [<RestEase.Path "id">] id : int * ?ct : System.Threading.CancellationToken -> unit System.Threading.Tasks.Task

    /// List the current user's organizations
    [<RestEase.Get "user/orgs">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgListCurrentUserOrgs :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Organization list System.Threading.Tasks.Task

    /// List the repos that the authenticated user owns
    [<RestEase.Get "user/repos">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentListRepos :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Repository list System.Threading.Tasks.Task

    /// Create a repository
    [<RestEase.Post "user/repos">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract CreateCurrentUserRepo :
        [<RestEase.Body>] body : CreateRepoOption * ?ct : System.Threading.CancellationToken ->
            Repository System.Threading.Tasks.Task

    /// Get user settings
    [<RestEase.Get "user/settings">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract GetUserSettings : ?ct : System.Threading.CancellationToken -> UserSettings list System.Threading.Tasks.Task

    /// Update user settings
    [<RestEase.Post "user/settings">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UpdateUserSettings :
        [<RestEase.Body>] body : UserSettingsOptions * ?ct : System.Threading.CancellationToken ->
            UserSettings list System.Threading.Tasks.Task

    /// The repos that the authenticated user has starred
    [<RestEase.Get "user/starred">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentListStarred :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Repository list System.Threading.Tasks.Task

    /// Whether the authenticated is starring the repo
    [<RestEase.Get "user/starred/{owner}/{repo}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentCheckStarring :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Unstar the given repo
    [<RestEase.Delete "user/starred/{owner}/{repo}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentDeleteStar :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Star the given repo
    [<RestEase.Put "user/starred/{owner}/{repo}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentPutStar :
        [<RestEase.Path "owner">] owner : string *
        [<RestEase.Path "repo">] repo : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get list of all existing stopwatches
    [<RestEase.Get "user/stopwatches">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserGetStopWatches :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            StopWatch list System.Threading.Tasks.Task

    /// List repositories watched by the authenticated user
    [<RestEase.Get "user/subscriptions">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentListSubscriptions :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Repository list System.Threading.Tasks.Task

    /// List all the teams a user belongs to
    [<RestEase.Get "user/teams">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserListTeams :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Team list System.Threading.Tasks.Task

    /// List the current user's tracked times
    [<RestEase.Get "user/times">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCurrentTrackedTimes :
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        [<RestEase.Query "since">] since : string *
        [<RestEase.Query "before">] before : string *
        ?ct : System.Threading.CancellationToken ->
            TrackedTime list System.Threading.Tasks.Task

    /// Search for users
    [<RestEase.Get "users/search">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserSearch :
        [<RestEase.Query "q">] q : string *
        [<RestEase.Query "uid">] uid : int *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get a user
    [<RestEase.Get "users/{username}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserGet :
        [<RestEase.Path "username">] username : string * ?ct : System.Threading.CancellationToken ->
            User System.Threading.Tasks.Task

    /// List the given user's followers
    [<RestEase.Get "users/{username}/followers">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserListFollowers :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            User list System.Threading.Tasks.Task

    /// List the users that the given user is following
    [<RestEase.Get "users/{username}/following">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserListFollowing :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            User list System.Threading.Tasks.Task

    /// Check if one user is following another user
    [<RestEase.Get "users/{username}/following/{target}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCheckFollowing :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Path "target">] target : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Get a user's heatmap
    [<RestEase.Get "users/{username}/heatmap">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserGetHeatmapData :
        [<RestEase.Path "username">] username : string * ?ct : System.Threading.CancellationToken ->
            UserHeatmapData list System.Threading.Tasks.Task

    /// List the given user's public keys
    [<RestEase.Get "users/{username}/keys">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserListKeys :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Query "fingerprint">] fingerprint : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            PublicKey list System.Threading.Tasks.Task

    /// List a user's organizations
    [<RestEase.Get "users/{username}/orgs">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgListUserOrgs :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Organization list System.Threading.Tasks.Task

    /// Get user permissions in organization
    [<RestEase.Get "users/{username}/orgs/{org}/permissions">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract OrgGetUserPermissions :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Path "org">] org : string *
        ?ct : System.Threading.CancellationToken ->
            OrganizationPermissions System.Threading.Tasks.Task

    /// List the repos owned by the given user
    [<RestEase.Get "users/{username}/repos">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserListRepos :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Repository list System.Threading.Tasks.Task

    /// The repos that the given user has starred
    [<RestEase.Get "users/{username}/starred">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserListStarred :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Repository list System.Threading.Tasks.Task

    /// List the repositories watched by a user
    [<RestEase.Get "users/{username}/subscriptions">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserListSubscriptions :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            Repository list System.Threading.Tasks.Task

    /// List the authenticated user's access tokens
    [<RestEase.Get "users/{username}/tokens">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserGetTokens :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Query "page">] page : int *
        [<RestEase.Query "limit">] limit : int *
        ?ct : System.Threading.CancellationToken ->
            AccessToken list System.Threading.Tasks.Task

    /// Create an access token
    [<RestEase.Post "users/{username}/tokens">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserCreateToken :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Body>] body : CreateAccessTokenOption *
        ?ct : System.Threading.CancellationToken ->
            AccessToken System.Threading.Tasks.Task

    /// delete an access token
    [<RestEase.Delete "users/{username}/tokens/{token}">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract UserDeleteAccessToken :
        [<RestEase.Path "username">] username : string *
        [<RestEase.Path "token">] token : string *
        ?ct : System.Threading.CancellationToken ->
            unit System.Threading.Tasks.Task

    /// Returns the version of the Gitea application
    [<RestEase.Get "version">]
    [<RestEase.Header("Content-Type", "json")>]
    abstract GetVersion : ?ct : System.Threading.CancellationToken -> ServerVersion System.Threading.Tasks.Task
