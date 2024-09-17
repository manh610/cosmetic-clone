package com.cosmetic.gg.dto.response.address;

import javax.validation.constraints.NotNull;

import com.cosmetic.gg.common.enums.EAddressType;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class AddressObjectResponse extends BaseModel{
	
	private String provinceId;
	
	private String provinceFullName;

	private String districtId;
	
	private String districtFullName;

	private String wardId;
	
	private String wardFullName;
	
	private String detail;
	
	@NotNull(message = "Default can not null")
	private boolean isDefault;
	
	private EStatus status;
	
	private String fullName;

	private String phone;	
	
	private EAddressType addressType;
	
	private String objectId;
	
	private String branch;
}
