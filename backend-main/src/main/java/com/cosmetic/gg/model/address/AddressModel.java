package com.cosmetic.gg.model.address;
import javax.persistence.Column;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.validation.constraints.NotNull;

import com.cosmetic.gg.common.enums.EAddressType;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.model.BaseModel;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@JsonInclude(JsonInclude.Include.NON_NULL)
@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class AddressModel extends BaseModel{

	private static final long serialVersionUID = 7032972419462740660L;

	private String provinceId;
	
	private String provinceFullName;

	private String districtId;
	
	private String districtFullName;

	private String wardId;
	
	private String wardFullName;
	
	private String userId;
	
	private String detail;
	
	@NotNull(message = "Default can not null")
	private boolean isDefault;
	
	private EStatus status;
	
	private String fullName;

	private String phone;	
	
	private EAddressType addressType;
}
