package com.cosmetic.gg.entity.supply;

import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import com.cosmetic.gg.entity.EntityCommon;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "import")
@Entity
@Getter
@Setter
public class Import extends EntityCommon{

	@NotNull(message = "Code can not null")
	@Column(name = "code", unique = true)
	private String code;
	
	@Column(name = "import_date")
	private LocalDateTime importDate;
	
	@Column(name = "vat")
	private int vat;
	
	@Column(name = "representative_name")
	private String representativeName;
	
	@Column(name = "representative_phone")
	private String representativePhone;
	
	@Column(name = "representative_email")
	private String representativeEmail;
	
	@Column(name = "supplier_id")
	private String supplierId;
	
	@Column(name = "supplier_address_id")
	private String supplierAddressId;
}
