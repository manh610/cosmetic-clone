package com.cosmetic.gg.entity.address;

import javax.persistence.*;
import javax.validation.constraints.NotNull;

import com.cosmetic.gg.common.enums.EAddressType;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.entity.EntityBase;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "address", indexes = {
    @Index(name = "idx_address_province_id", columnList = "province_id"),
    @Index(name = "idx_address_district_id", columnList = "district_id"),
    @Index(name = "idx_address_ward_id", columnList = "ward_id"),
    @Index(name = "idx_address_detail", columnList = "detail")
})
@Entity
@Getter
@Setter
public class Address extends EntityBase{
	private static final long serialVersionUID = 1L;
	
	@Column(name = "province_id")
	private String provinceId;
	
	@Column(name = "district_id")
	private String districtId;
	
	@Column(name = "ward_id")
	private String wardId;
	
	@Column(name = "detail")
	private String detail;	
	
	@Column(name = "is_default")
	private boolean isDefault;
	
	@Column(name = "status")
	@Enumerated(EnumType.STRING)
	private EStatus status;
	
	@Column(name = "full_name")
	private String fullName;
	
	@Column(name = "phone")
	private String phone;	
	
	@Column(name = "address_type")
	@Enumerated(EnumType.STRING)
	private EAddressType addressType;
}
