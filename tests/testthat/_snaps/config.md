# makeConfig works

    Code
      writeLines(read_utf8(proj_path("config.yml")))
    Output
      # Config File for testprojcb827726ad9
      
      default:
        projectName: testprojcb827726ad9
      
      # Config block for test
      
      test:
        databaseName: synpuf_110k
        dbms: !expr keyring::key_get('test_dbms', keyring = 'testprojcb827726ad9')
        user: !expr keyring::key_get('test_user', keyring = 'testprojcb827726ad9')
        password: !expr keyring::key_get('test_password', keyring = 'testprojcb827726ad9')
        connectionString: !expr keyring::key_get('test_connectionString', keyring = 'testprojcb827726ad9')
        cdmDatabaseSchema: !expr keyring::key_get('test_cdmDatabaseSchema', keyring = 'testprojcb827726ad9')
        vocabDatabaseSchema: !expr keyring::key_get('test_vocabDatabaseSchema', keyring = 'testprojcb827726ad9')
        workDatabaseSchema: !expr keyring::key_get('test_workDatabaseSchema', keyring = 'testprojcb827726ad9')
        role: !expr keyring::key_get('test_role', keyring = 'testprojcb827726ad9')
        cohortTable: testprojcb827726ad9_synpuf_110k

