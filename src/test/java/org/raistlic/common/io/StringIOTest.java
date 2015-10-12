package org.raistlic.common.io;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.raistlic.common.precondition.InvalidParameterException;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import static org.fest.assertions.Assertions.assertThat;

/**
 * @author Lei Chen (2015-10-12)
 */
@RunWith(JUnit4.class)
public class StringIOTest {

  @Test(expected = InvalidParameterException.class)
  public void readAllWithNullInputStream() throws Exception {

    StringIO.readAll(null);
  }

  @Test(expected = IOException.class)
  public void readAllWhenInputStreamThrowsException() throws Exception {

    InputStream inputStream = new InputStream() {

      @Override
      public int read() throws IOException {

        throw new IOException("Test Exception");
      }
    };

    StringIO.readAll(inputStream);
  }

  @Test
  public void readAllExpected() throws Exception {

    String fixture = "9fe641c3-d760-403a-855d-da368292a73d\n" +
        "3240521d-271d-4566-bc75-4cab4472a7bc\n" +
        "c96fcd38-d653-480e-8f19-1a81fc16fdfc\n" +
        "dad6c58f-f537-4230-a656-0031036b353d\n" +
        "793f5dc1-96f9-4c75-9341-857c39b30929\n" +
        "0f68f416-1b5a-4191-9bad-8f3dea029042\n" +
        "1712ff51-6f97-46b2-854b-1e56219802e6\n" +
        "e4f6dc12-03e7-40ec-9b25-cea4f5b7daaa\n" +
        "e2e15173-19ba-4463-a3bd-34efa2124300\n" +
        "b2601c2f-c87d-4d40-93bc-18631fe36049\n" +
        "2736b064-bad2-4e61-addd-9810620ee8a1\n" +
        "d91e0607-4b74-47e6-89d9-6e75589edc6a\n" +
        "7ef134d5-78a0-4fc7-bc68-6ae611e5f793\n" +
        "44be4de2-50b4-41f4-a20f-df9377925d47\n" +
        "f4b610ea-49b9-4a87-b813-fb8409e7caf3\n" +
        "e4873089-2e62-4581-81bf-d688d372c2fa\n" +
        "e174e215-1144-4c0d-8628-d82ecbf610f0\n" +
        "8776a2d6-68c1-4f97-ba93-4ccc931bf60f\n" +
        "88d7aaa3-021a-4c97-87d9-6cd729e8449b\n" +
        "d0d8e1ae-16be-4564-b269-32c44e14211b\n" +
        "afc00c2b-c1ca-4c70-8a03-819c98cf2b74\n" +
        "7fee413c-7cc1-42cf-a302-32208785eb81\n" +
        "50914680-173c-4e41-af9a-84d7c82d4893\n" +
        "a530f435-c03c-414e-a5c8-6a216f954aad\n" +
        "563e90ce-87a8-4a94-994b-d80b4f721fea\n" +
        "db97936a-b12a-42d8-af49-a2ac441eb632\n" +
        "07a253b4-f202-4b07-a7b3-4f232aac1dfe\n" +
        "d70b117c-6df1-451d-9294-d73958565eda\n" +
        "16e1b431-8df1-4aec-b3e6-a479f72c4ee4\n" +
        "f019107b-663f-4f98-af9a-1a2c0eb6e535\n" +
        "60007d71-321e-42b7-9ce0-b8e4db237bdb\n" +
        "4f951897-104e-4965-abe4-19c6020a6a2a\n" +
        "cc591bca-e03a-4ac0-8a4e-1e7ef8ebcb68\n" +
        "30d1975e-1468-4142-a2e0-39dd6a7bda8a\n" +
        "b9944656-3d0c-4abc-bd7a-090c06cc2ef7\n" +
        "bd1eff64-9647-4299-8450-3987723a178c\n" +
        "2c70852b-c6e4-44cc-be9d-e396931b85aa\n" +
        "03fc9440-a1f7-42dd-8777-cc616139619b\n" +
        "9eb20c3a-5f99-4afe-8c4b-87d1816b9947\n" +
        "dc2e44f9-6947-41da-923a-0881efcc0c82\n" +
        "c33551f7-62bf-4874-967b-3c0a866082d8\n" +
        "679dcaf3-b277-4525-9c33-982aa5a09c07\n" +
        "7a5b4328-89bf-4644-823e-64c3131e43f3\n" +
        "d764b9ad-4314-43ad-a76c-810d7202e43b\n" +
        "c8bebb3d-b419-4454-b403-ca871b217bc3\n" +
        "0f3825a4-2a42-4061-9a90-0605bff1d3a1\n" +
        "423bdcf8-a35e-4d85-a18a-33825d88e071\n" +
        "639507e5-efa3-4e0a-b5ea-37483504de94\n" +
        "55ffb764-4f10-49e1-a9d4-a0360bfebc79\n" +
        "d7528aa9-7a46-41d9-ae2e-38d2a6631f80\n" +
        "8e5bcd61-cb21-4953-a216-0c4604de260c\n" +
        "4695dc6a-acca-44c2-a977-ebd80c1b7571\n" +
        "766117dd-8e05-465a-9436-886390c156cd\n" +
        "3428655f-7992-46e2-a7ea-cb10f888e8a4\n" +
        "0e25d218-40b2-40ea-9d70-c5ce37ed9de7\n" +
        "5f92c538-a1ed-47d9-906e-9d2efc1fa430\n" +
        "42560c0f-044a-4531-981e-13cd26288d73\n" +
        "a82e2cbe-723a-4c51-9b50-01449f2ac8ca\n" +
        "529f6297-a179-47a5-869e-f0640d89a7cd\n" +
        "d61e2032-f0bc-46d0-9912-a348582b74ea\n" +
        "973cc6e0-553a-4283-9172-7378525f3d55\n" +
        "f3fac993-b524-41a2-8e11-464f056415c2\n" +
        "83c9d475-8b17-4e42-96d6-ac38f21f4eb7\n" +
        "05980686-ca73-40ae-966b-7a085f8fea3c\n" +
        "42a80e2c-f9f4-4fba-9ae5-15232260748b";

    InputStream inputStream = new ByteArrayInputStream(fixture.getBytes());
    String actual = StringIO.readAll(inputStream);
    assertThat(actual).isEqualTo(fixture);
  }
}
